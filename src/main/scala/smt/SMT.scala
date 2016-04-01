package smt

import sbt._
import sbt.Keys._
import java.io.File
import smt.db.{MetaDatabase, DatabaseId, Database}
import smt.migration.Migration
import smt.report.Reporter

object SMT extends Plugin {

  import MigrationHandling._

  lazy val globalSmtSettings = Seq(
    migrationsSource <<= (sourceDirectory in Compile) / "migrations",
    scriptSource <<= baseDirectory,
    allowRollback := false,
    runTests := true,
    initMigration := None
  )

  lazy val smtSettings = Seq(
    transformedMigrations <<= (migrations, transformations, transformations) map transformedMigrationsImpl,
    showHashes <<= (transformedMigrations, initMigration, streams) map showHashesImpl,
    showDbState <<= (metaDatabase, transformedMigrations, initMigration, streams) map SMTImpl.showDbState,
    applyMigrations <<= inputTask((argTask: TaskKey[Seq[String]]) => (argTask, metaDatabase, databases, transformedMigrations, initMigration, allowRollback, runTests, reporters, user, streams) map SMTImpl.applyMigrations),
    migrateTo <<= inputTask((argTask: TaskKey[Seq[String]]) => (argTask, metaDatabase, databases, transformedMigrations, initMigration, allowRollback, runTests, reporters, user, streams) map SMTImpl.migrateTo),
    showLatestCommon <<= (metaDatabase, transformedMigrations, initMigration, streams) map SMTImpl.showLatestCommon,
    runScript <<= inputTask((argTask: TaskKey[Seq[String]]) => (argTask, scriptSource, metaDatabase, databases, streams) map SMTImpl.runScript),
    reporters := Seq[Reporter](),
    user := System.getProperty("user.name")
  )

  val migrationsSource = SettingKey[File]("migrations-source", "base-directory for migration files")

  val scriptSource = SettingKey[File]("script-source", "base directory for migration-independent db-scripts")

  val initMigration = SettingKey[Option[(Int, String)]]("init-migration", "an optional ititial migration to base the migrations in the repository on")

  val migrations = TaskKey[Seq[Migration]]("migrations", "sequence of migrations")

  val allowRollback = SettingKey[Boolean]("allow-rollback", "indicates if migrations can be rolled back")

  val runTests = SettingKey[Boolean]("run-tests", "indicates if tests should be run after applying a migration")

  val transformedMigrations = TaskKey[Seq[Migration]]("transformed-migrations", "transformed migrations")

  val transformations = SettingKey[Seq[Transformation]]("transformations", "transformations of the migrations")

  val showHashes = TaskKey[Unit]("show-hashes", "show the hash sums of the migrations")

  val metaDatabase = SettingKey[MetaDatabase]("meta-database", "implementation of db abstraction. The db that holds the smt metadata")

  val databases = SettingKey[Map[DatabaseId, Database]]("databases", "maps databaseIds to the databases where migrations will be applied")

  val showDbState = TaskKey[Unit]("show-db-state", "show the state of the db")

  val showLatestCommon = TaskKey[Unit]("show-latest-common", "show the latest common migration")

  val applyMigrations = InputKey[Unit]("apply-migrations", "apply the migrations to the DB")

  val migrateTo = InputKey[Unit]("migrate-to", "move db to the specified migration")

  val runScript = InputKey[Unit]("run-script", "run a script against a database")

  val reporters = SettingKey[Seq[Reporter]]("reporters", "sequence of reporters to notify about db changes")

  val user = SettingKey[String]("user", "the operating user")
}
