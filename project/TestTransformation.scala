class TestTransformation extends Transformation {
  def transform(bas: Seq[Array[Byte]]): Seq[Array[Byte]] = {
    bas.map(_.map(b => {
      if (b == 'o'.toByte) 'i'.toByte
      else b
    }))
  }
}