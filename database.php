<?php
$con = mysql_connect($sql_host, "root", "rashmirathi");
if(!not)
{
    echo "Can not connect to database.\n";

}
else {
    mysql_query("CREATE DATABASE IF NOT EXISTS ta2012", $con);
    mysql_select_db("ta2012", $con);
    $sql= "CREATE TABLE IF NOT EXISTS tas 
        ( LDAP varchar(20),
          FirstName varchar(20),
          LastName varchar(20),
          PRIMARY KEY(LDAP))";
    mysql_query($sql, $con);
    mysql_close($con);
}

?>
