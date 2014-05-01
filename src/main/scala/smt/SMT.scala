package smt

import sbt._
import sbt.Keys._
import java.io.File
import smt.db.Database
import smt.migration.Migration

object SMT extends Plugin with DBHandling {

  import MigrationHandling._


  lazy val globalSmtSettings = Seq(
    migrationsSource <<= (sourceDirectory in Compile) / "migrations",
    allowRollback := false,
    runTests := true
  )

  lazy val smtSettings = Seq(
    transformedMigrations <<= (migrations, transformations) map transformedMigrationsImpl,
    showHashes <<= (transformedMigrations, streams) map showHashesImpl,
    showDbState <<= (database, streams) map showDbStateImpl,
    applyMigrations <<= (database, transformedMigrations, allowRollback, runTests, streams) map applyMigrationsImpl,
    migrateTo <<= inputTask((argTask: TaskKey[Seq[String]]) => (argTask, database, transformedMigrations, allowRollback, runTests, streams) map migrateToImpl),
    showLatestCommon <<= (database, transformedMigrations, streams) map showLatestCommonImpl
  )

  val migrationsSource = SettingKey[File]("migrations-source", "base-directory for migration files")

  val migrations = TaskKey[Seq[Migration]]("migrations", "sequence of migrations")

  val allowRollback = SettingKey[Boolean]("allow-rollback", "indicates if migrations can be rolled back")

  val runTests = SettingKey[Boolean]("run-tests", "indicates if tests should be run after applying a migration")

  val transformedMigrations = TaskKey[Seq[Migration]]("transformed-migrations", "transformed migrations")

  val transformations = SettingKey[Seq[Transformation]]("transformations", "transformations of the migrations")

  val showHashes = TaskKey[Unit]("show-hashes", "show the hash sums of the migrations")

  val database = SettingKey[Database]("database", "implementation of db abstraction")

  val showDbState = TaskKey[Unit]("show-db-state", "show the state of the db")

  val showLatestCommon = TaskKey[Unit]("show-latest-common", "show the latest common migration")

  val applyMigrations = TaskKey[Unit]("apply-migrations", "apply the migrations to the DB")

  val migrateTo = InputKey[Unit]("migrate-to", "move db to the specified migration")

  private def migrateToImpl(args: Seq[String], db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, s: TaskStreams) {
    args match {
      case Seq(target) => {
        val mst = ms.reverse.dropWhile(_.name != target).reverse
        if (mst.isEmpty) throw new Exception("No migration named '" + target + "' defined")
        else {
          SMT.applyMigrationsImpl(db, mst, arb, runTests, s)
        }
      }
      case Seq() => throw new Exception("Name of a migration expected.")
      case _ => throw new Exception("Too many arguments. Name of a migration expected.")
    }
  }
}
