package smt

import java.sql.{Connection => JConnection, _}

class OracleDbWithOutput(connection: => JConnection) extends OracleDatabase(connection) {

  override def applyScript(script: Script, direction: Direction): (Option[Failure], Database) = effectExceptionToFailure {
    println("applying " + direction + " script: " + script)

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
        println( stm.getString(3) )
      } while (stm.getInt(2) != 1)
    })

    withStatement(cnx)(_.execute("begin dbms_output.disable; end;"))
  }
}
