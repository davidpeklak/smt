# SMT
Smt is a database migrations tool that is implemented as a plugin for [sbt](http://www.scala-sbt.org). Smt stands either for 'sbt migrations tool' or for 'simple migrations tool'.

Smt is ideally used by teams or single developers who use scala and sbt on the application side, and have to do a lot of database work, maybe with a dedicated database-developer. A basic set-up is relatively straight-forward, and can be done by any developer without a specific scala / sbt background. For more complex set-ups, it is certainly helpful to have
some-one with a bit of sbt familiarity at hand.

## Why smt?
Smt is simple to use (especially if you are familiar with sbt), and is simple in its implementation, so it can easily be enhanced with your own developments. The code is strictly separated into two parts: a core part than can be easily reused
as a library, and a wrapping part that makes the whole thing an sbt-plugin.

## Getting started
The easiest way to get started is to check-out the [smt-usage](https://github.com/davidpeklak/smt-usage) project. It serves as an example of an (amlost) minimal set-up that demonstrates the essential features. It works with two H2 in-memory database instances, so that you don't need to provide your own database instances to experiment with the tool. The smt-usage projet will soon get its own documentation that explains you what's going on.

# Reference manual
## Script
A script is a string that can be applied to a database instance to change it's structure. In the typical case of relational databases, this will be one ore more sql-statements. In order to change the structure of your database instance, you apply a script to it, this script is called an 'up'-script. The corresponding 'down'-script will roll back the effect of the 'up'-script.
## Migration
A migration is a sequence of scripts, or rather, two sequences of scripts: one sequence of 'up'-scripts that change the structure of your database, and one sequence of 'down'-scripts that roll back these changes. A migration also has a name. It is ultimatly up to you how to use that name but conventionally the name represents the version of the database structure that is the result of applying the 'up'-scripts of the migration to the database. `smt.Migration` is a scala trait and smt lets you construct instances of migrations with `smt.FileMigration(<name>, <directories>)`, where 'directories' is a sequence of directories, in each of which smt expects one 'up.sql' script and one 'down.sql' script 
## Migrations (plural!)
`migrations` is an sbt setting that tells smt which migations you intend to apply to your database-instance. You have to assign a value of type `Seq[smt.Migration]`to it.
Use the task `show-migations` to verify the migrations that you have defined. 

## Database instance
You have to define a database instance for smt, so that it knows what to apply the migrations to. You do this by giving the setting `database` a value. This value must be of type `smt.Database`, which is a scala trait, and smt provides two classes that implement that trait: `smt.OracleDatabase` and `smt.H2Database`. Both take a `=> java.sql.Connection` as constructor argument (check out the smt-usage project to see how to set this value in a project).
## State of the database instance
If you defined two migrations 'm1' and 'm2', and you applied these two migrations to your database instance using smt, the state of your database instance (from an smt point of view) is the sequence (m1, m2). Smt stores information of what migrations it has applied to the database instance in dedicated tables. The sbt-task `show-db-state` will show you this sequence. (On a fresh database instance, i.e. on one where no migrations have been applied before with smt, this sequence will be empty, so you will not see much.)

## Changes to migrations
So let's assume you have applied the migrations m1 and m2 to your database, and afterwards you figure out that you actually should have done something different in m2. What do you do? You roll back m2 on the database instance (by applying the 'down'-scripts of m2 to the database instance), change m2 and then apply the (changed) 'up'-scripts to the database instance. And actually, you don't have to do this manually, because, smt will do this for you. Here is how:
## Hashes
When you apply a migration to a database instance, smt generates a hash-sum of this migration. This hash-sum is calculated from the contents of the scripts of the migration, and from the hash-sum of the predecessor of the migration. I.e. in the exemplary sequence of migrations that we introduced above (m1, m2), if m1 changes, the hash of m1 changes, and therefore the hash of m2 changes as well. Smt stores the hashes of the migrations that have been applied to a database instance in a table. So if you apply (m1, m2) to your database instance, and later you change m2, smt will notice that the hash that is stored for m2 when you applied it (before the change) differs from the hash of m2 after the change. Smt also stores the 'down'-scripts of a migation in a table, so that if you change a migration, it will still have available the 'down'-scripts of the migration before the change.
## Finding the latest common migration
To compare the database state with the migrations that you have currently defined, you can use the `find-latest-common` task. As the name suggests, it will show you the latest common migraion of the migrations that have been applied to the database instance and your current sequence of migraitons. In our example, if you applied (m1, m2) to the database instance and then changed m2, the latest common migration will be m1.
## Applying migrations to a database instance
You apply migratiobs to a database instance with the `apply-migrations` task. Here is what it will do:

1. Find the latest common migration
2. Roll back all migrations that succeed the latest common migation on the database instance
3. Apply all migrations that succeed the latest common migration in your current sequence of migrations

## Environments
Typically, you will not deal with only one database instance, but you will want to apply your migrations to, say, one or more dev-instances, a uat-instance, and eventually a prod-instance.

You can use sbt configurations to model your different environments. The [smt-usage](https://github.com/davidpeklak/smt-usage) project shows you how to do that. 
## Transformations
The scripts that you want to apply to your different environments are hopefully (almost) identical, but sometimes, tiny little pieces will have to be environment-specific. To make that possible, smt lets you define transformations that will be applied to your scripts before they get applied to your database instance. You do so by assigning a value of type `Seq[smt.MigrationHandling#Transformation]` to the sbt setting `transformations`, where `smt.MigrationHandling#Transformation` is simply a type alias for `String => String` and means 'script in - transformed script out'. Typically, a transformation will just replace some place-holder in your scripts with some concrete name. Since you want that name to be different in different environments, you will want to define your `transformations` sbt-configuration specific. This whole paragraph will make sense to you once you checket out how the `transformations` are defined in the [smt-usage](https://github.com/davidpeklak/smt-usage) project. One more thng: hashes are calculated *after* transformations have been applied, so be careful when you change the transformations in your prod environment.