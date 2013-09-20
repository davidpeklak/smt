package smt

import org.scalacheck.Gen
import org.scalacheck.Gen._

object MigrationGen {
  val migGen: Gen[Migration] = {
    for (name <- alphaStr;
         numberOfScripts <- choose(1, 10);
         ups <- sequence[List, String](List.fill(numberOfScripts)(alphaStr));
         downs <- sequence[List, String](List.fill(numberOfScripts)(alphaStr)))
    yield Migration(name, ups, downs)
  }
}
