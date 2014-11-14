package smt.db.impl

import java.sql.{Connection => JConnection}
import smt.db.Database
import smt.migration.{Script, Direction}
import sbt.Logger
import scalaz.\/

class OracleDatabaseWithOutput(connection: => JConnection) extends SqlDatabase(
new OracleConnectionWithOutput(connection)
)

class OracleConnectionWithOutput(connection: JConnection) extends OracleConnection(connection) {
  import SqlDatabase._
  import SqlConnection._

  private def doApplyScript(logger: Logger)(script: Script) {
    withStatement(cnx)(_.execute("begin dbms_output.enable(1000000); end;"))

    withStatement(cnx)(_.execute(script.content))

    withCallableStatement(cnx,
      """declare
            l_line varchar2(255);
           l_done number;
            l_buffer long;
        begin
          loop
            exit when length(l_buffer)+255 > :maxbytes OR l_done = 1;
            dbms_output.get_line( l_line, l_done );
            l_buffer := l_buffer || l_line || chr(10);
          end loop;
         :done := l_done;
         :buffer := l_buffer;
        end;"""
    ) ( stm => {
      stm.registerOutParameter( 2, java.sql.Types.INTEGER )
      stm.registerOutParameter( 3, java.sql.Types.VARCHAR )

      do
      {
        stm.setInt( 1, 32000 )
        stm.executeUpdate()
        logger.info( stm.getString(3) )
      } while (stm.getInt(2) != 1)
    })

    withStatement(cnx)(_.execute("begin dbms_output.disable; end;"))
  }

  override def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit = fromTryCatch {
    logger.info("applying " + direction + " script: " + script)
    doApplyScript(logger)(script)
  }

  override def testScript(logger: Logger)(script: Script): String \/ Unit = fromTryCatch {
    logger.info("applying test script: " + script)
    doApplyScript(logger)(script)
  }
}
