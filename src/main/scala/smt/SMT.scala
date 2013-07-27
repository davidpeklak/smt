package smt

import sbt._
import sbt.Keys._
import java.io.File

object SMT extends Plugin with MigrationHandling with DBHandling {

  lazy val smtSettings = Seq(
    migrationsSource <<= (sourceDirectory in Compile) / "migrations",
    transformedMigrations <<= (migrations, transformations) map transformedMigrationsImpl,
    showHashes <<= (transformedMigrations, streams) map showHashesImpl,
    showDbState <<= (database, streams) map showDbStateImpl,
    applyMigrations <<= (database, transformedMigrations, streams) map applyMigrationsImpl,
    showLatestCommon <<= (database, transformedMigrations, streams) map showLatestCommonImpl
  )

  val migrationsSource = SettingKey[File]("migrations-source", "base-directory for migration files")

  val migrations = TaskKey[Seq[Migration]]("migrations", "sequence of migrations")

  val transformedMigrations = TaskKey[Seq[Migration]]("transformed-migrations", "transformed migrations")

  val transformations = SettingKey[Seq[Transformation]]("transformations", "transformations of the migrations")

  val showHashes = TaskKey[Unit]("show-hashes", "show the hash sums of the migrations")

  val database = SettingKey[Database]("database", "implementation of the database abstraction")

  val showDbState = TaskKey[Unit]("show-db-state", "show the state of the db")

  val showLatestCommon = TaskKey[Unit]("show-latest-common", "show the latest common migration")

  val applyMigrations = TaskKey[Unit]("apply-migrations", "apply the migrations to the DB")
}