class TestTransformation extends Transformation {
  def transform(bas: Seq[String]): Seq[String] = {
    bas.map(_.map(b => {
      if (b == 'o') 'i'
      else b
    }))
  }
}