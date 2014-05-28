package smt

import sbt._
import sbt.Keys._
import java.io.File
import smt.db.Database
import smt.migration.Migration
import smt.report.Reporter

object SMT extends Plugin {

  import MigrationHandling._

  lazy val globalSmtSettings = Seq(
    migrationsSource <<= (sourceDirectory in Compile) / "migrations",
    scriptSource <<= baseDirectory,
    allowRollback := false,
    runTests := true
  )

  lazy val smtSettings = Seq(
    transformedMigrations <<= (migrations, transformations) map transformedMigrationsImpl,
    showHashes <<= (transformedMigrations, streams) map showHashesImpl,
    showDbState <<= (database, streams) map SMTImpl.showDbState,
    applyMigrations <<= (database, transformedMigrations, allowRollback, runTests, reporters, streams) map SMTImpl.applyMigrations,
    migrateTo <<= inputTask((argTask: TaskKey[Seq[String]]) => (argTask, database, transformedMigrations, allowRollback, runTests, reporters, streams) map SMTImpl.migrateTo),
    showLatestCommon <<= (database, transformedMigrations, streams) map SMTImpl.showLatestCommon,
    runScript <<= inputTask((argTask: TaskKey[Seq[String]]) => (argTask, scriptSource, database, streams) map SMTImpl.runScript),
    reporters := Seq[Reporter]()
  )

  val migrationsSource = SettingKey[File]("migrations-source", "base-directory for migration files")

  val scriptSource = SettingKey[File]("script-source", "base directory for migration-independent db-scripts")

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

  val runScript = InputKey[Unit]("run-script", "run a script against the database")

  val reporters = SettingKey[Seq[Reporter]]("reporters", "sequence of reporters to notify about db changes")
}
