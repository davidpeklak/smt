package smt

import org.scalacheck.Gen
import org.scalacheck.Gen._

object MigrationGen {
  val migGen: Gen[Migration] = {
    for (name <- alphaStr;
         numberOfScripts <- choose(1, 10);
         ups <- sequence[List, Script](List.fill(numberOfScripts)(alphaStr.map(Script("name", _))));
         downs <- sequence[List, Script](List.fill(numberOfScripts)(alphaStr.map(Script("name", _)))))
    yield Migration(name, ups, downs)
  }
}
