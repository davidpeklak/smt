database := new TestDatabase

migrations <<= (migrationsSource) map ( m => Seq(
  FileMigration("0.0.1", Seq(m / "s1", m / "s2")),
  FileMigration("0.0.2", Seq(m / "s3"))
))

transformations := Seq(new TestTransformation)