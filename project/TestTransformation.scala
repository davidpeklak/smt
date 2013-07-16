class TestTransformation extends Transformation {
  def transform(s: String): String = {
    s.map(c => {
      if (c == 'o') 'i'
      else c
    })
  }
}