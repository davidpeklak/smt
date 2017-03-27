package smt

import sbt.Logger
import smt.db.{MetaConnection, Database, DatabaseId, Connection}
import smt.migration._
import smt.util.EitherHaerte.EitherSyntax._
import smt.util.SeqHaerte.SeqSyntax._

import scalaz.{-\/, \/}
import scalaz.Scalaz._

object MulipleDatabasesHandling {

  def withConnection(dbId: DatabaseId, effect: Connection => String \/ Unit)(databases: Map[DatabaseId, Database], logger: Logger): String \/ Unit = {
    for {
      database <- toRight(databases.get(dbId))(s"Database $dbId is not mapped")
      connection <- database.connection()
      _ <- {
        effect(connection)
          .andFinally(connection.close(logger))
      }
    } yield ()
  }

  def connectAndAddDownMoveStateOf(mi: MigrationInfo)(effect: (Connection, DownMoveStateHolder) => String \/ Unit)(databases: Map[DatabaseId, Database], logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    nms.addDownMoveStateOf(mi.name)(dms => {
      withConnection(mi.dbId, c => effect(c, dms))(databases, logger)
    })
  }

  def connectAndAddUpMoveStateOf(migration: Migration)(effect: (Connection, UpMoveStateHolder) => String \/ Unit)(databases: Map[DatabaseId, Database], logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    nms.addUpMoveStateOf(migration.name)(ums => {
      withConnection(migration.dbId, c => effect(c, ums))(databases, logger)
    })
  }

  def revertToLatestCommon(latestCommon: Option[Seq[Byte]], arb: Boolean, user: String, remark: String)(metaConnection: MetaConnection, databases: Map[DatabaseId, Database], logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    for {
      mis <- MetaConnectionHandling.migrationsToRevert(latestCommon)(metaConnection, logger)
      mids <- mis.travE(mi => MetaConnectionHandling.enrichMigrationWithDowns(mi)(metaConnection, logger))
      _ <- {
        if (mids.isEmpty || arb) mids.travE_(mid => {
          connectAndAddDownMoveStateOf(mid.mi)((connection, dms) => SingleConnectionHandling.revertMigration(mid, user, remark)(metaConnection, connection, logger, dms))(databases, logger, nms)
        })
        else -\/("Will not roll back migrations " + mids.map(_.mi.name).mkString(", ") + ", because allow-rollback is set to false")
      }
    }

      yield ()
  }

  def applyMigrations(ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, user: String, remark: String)(metaConnection: MetaConnection, databases: Map[DatabaseId, Database], logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    val mhs = MigrationHandling.hashMigrations(ms, imo)

    for {
      mis <- metaConnection.state(logger)
      lcho = MigrationHandling.latestCommon2(mis, mhs).map(_.db.hash)
      _ <- {
        if (lcho.isDefined || imo.isEmpty) for {
          _ <- revertToLatestCommon(lcho, arb, user, remark)(metaConnection, databases, logger, nms)
          _ <- doApplyMigrations(mhs, lcho, runTests, user, remark)(metaConnection, databases, logger, nms)
        } yield ()
        else -\/("Will not roll back migrations, because an initialMigration is set, and there are no common migrations.")
      }
    } yield ()
  }

  def doApplyMigrations(mhs: HashedMigrationSeq, latestCommon: Option[Seq[Byte]], runTests: Boolean, user: String, remark: String)(metaConnection: MetaConnection, databases: Map[DatabaseId, Database], logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    MigrationHandling.migrationsToApply(mhs, latestCommon).migs.travE_ {
      case (m, h) => connectAndAddUpMoveStateOf(m)((connection, ums) => {
        if (runTests) for {
          _ <- SingleConnectionHandling.applyMigration(m, h, user, remark)(metaConnection, connection, logger, ums)
          _ <- SingleConnectionHandling.testMigration(m)(connection, logger)
        } yield ()
        else SingleConnectionHandling.applyMigration(m, h, user, remark)(metaConnection, connection, logger, ums)
      })(databases, logger, nms)
    }
  }
}
