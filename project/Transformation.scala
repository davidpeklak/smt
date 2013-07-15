trait Transformation {
  def transform(bas: Seq[Array[Byte]]): Seq[Array[Byte]]
}