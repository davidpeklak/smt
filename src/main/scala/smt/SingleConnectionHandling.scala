package smt

import smt.util.Logger
import smt.DownMoveState._
import smt.db.{MetaConnection, Connection}
import smt.migration._
import smt.util.SeqHaerte.SeqSyntax._
import smt.util.EitherHaerte.EitherSyntax._
import scalaz.Scalaz._

import scalaz.{\/-, -\/, \/}

object SingleConnectionHandling {

  import MetaConnectionHandling.MigrationInfoWithDowns

  def testMigration(m: Migration)(connection: Connection, logger: Logger): String \/ Unit = {
    m.tests.travE_(_.run(connection)(logger))
  }


  def revertMigration(mid: MigrationInfoWithDowns, user: String, remark: String)(metaConnection: MetaConnection, connection: Connection, logger: Logger, dms: DownMoveStateHolder): String \/ Unit = {

    def applyScriptAndWrite(down: Script): String \/ Unit = {
      val r = connection.applyScript(logger)(down, Down)
      r match {
        case -\/(_) => dms.add(crashedDown(down))
        case \/-(()) => dms.add(appliedDown(down))
      }
      r
    }

    val r = metaConnection.removeDowns(logger)(mid.mi.hash) >>
      metaConnection.remove(logger)(mid.mi.hash) >>
      mid.downs.reverse.travE_(applyScriptAndWrite)

    r.onLeftDo(f => MetaConnectionHandling.rewriteMigration(mid, dms.dms, MigrationHandling.failHash(f), user, remark)(metaConnection, logger))
  }

  def applyMigration(m: Migration, hash: Seq[Byte], user: String, remark: String)(metaConnection: MetaConnection, connection: Connection, logger: Logger, ums: UpMoveStateHolder): String \/ Unit = {
    def finalize(downs: List[Script], hash: Seq[Byte]): String \/ Unit = {
      metaConnection.addDowns(logger)(hash, downs) >>
        metaConnection.add(logger, m.name, m.dbId, hash, MetaConnectionHandling.now, user, remark)
    }

    val r = m.groups.travE_(g => applyGroup(g)(connection, logger, ums))

    r match {
      case err@ -\/(f) => {
        finalize(ums.ums.downsToApply, MigrationHandling.failHash(f)) >>
          err
      }
      case \/-(()) => finalize(ums.ums.downsToApply, hash)
    }
  }

  def applyGroup(group: Group)(connection: Connection, logger: Logger, ums: UpMoveStateHolder): String \/ Unit = {
    def apl(up: Script): String \/ Unit = {
      val r = connection.applyScript(logger)(up, Up)
      r match {
        case -\/(_) => ums.add(UpMoveState.crashedUp(up))
        case \/-(()) => ums.add(UpMoveState.appliedUp(up))
      }
      r
    }

    val r = group.ups.travE_(apl)
    r match {
      case -\/(_) => {}
      case \/-(()) => {
        ums.add(UpMoveState.downsToApply(group.downs.toList))
        ums.add(UpMoveState.appliedUpsWithDowns(group.ups.toList))
      }
    }
    r
  }
}
