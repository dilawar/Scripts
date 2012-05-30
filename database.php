<h1>EE IITB Teaching Assistant Interface</h1>

<?php
include('error.php');
include('func.php');
session_start();

$ldap = $_SESSION['ldap'];
$init = $_SESSION['init'];
$ip = $init['db_ip'];
$pass = $init['db_pass'];
$user = $init['db_user'];

$con = mysql_connect($ip, $user, $pass);
if(!$con) {
	echo printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
	header("Refresh: 3, url=$base_url./eeta.php");
}
else 
{
	$_ldap = $_POST['ldap'];
	$_roll = $_POST['roll'];
	$_specialization = $_POST['specialization'];
	$_program = $_POST['program'];
	$_category = $_POST['category'];
	$_gradYear = $_POST['gradYear'];
	$_graduatingOn = $_POST['graduatingOn'];	
	
	/* check if entry is already there. */
	$query = sprintf("select * from student where ldap='%s'",
				mysql_real_escape_string($_ldap));
	$res = mysql_db_query("eestudents", $query, $con);
	if(!$res) {
		echo "Error in query :".mysql_error();
	}
	else {
		$entry = mysql_fetch_assoc($res);
		if(count($entry) > 0) 
		{
			echo "Updating your entry ...<br>";
			$query = sprintf("update student set ldap='%s', roll = '%s', program='%s', category='%s', graduatingOn='%s'"
				, mysql_real_escape_string($_ldap)
				, mysql_real_escape_string($_roll)
				, mysql_real_escape_string($_program)
				, mysql_real_escape_string($_category)
				, mysql_real_escape_string($_graduatingOn)
			);
			$res = mysql_db_query("eestudents", $query, $con);
			if(!$res) {
				echo "Failed to update your entry with error".mysql_error();
			}
			else {
				echo "<b> Successfully updated your entry <br>";
			}
		}
		else {
			echo "Inserting your entry into database ... <br>";
			$query = sprintf("insert into student (ldap, roll, program, category, graduatingOn) 
				values ('%s', '%s', '%s', '%s', '%s' )", 
				mysql_real_escape_string($_ldap)
				, mysql_real_escape_string($_roll)
				, mysql_real_escape_string($_program)
				, mysql_real_escape_string($_category)
				, mysql_real_escape_string($_graduatingOn)
			);
			$res = mysql_db_query("eestudents", $query, $con);

			if(!$res) {
				echo printErrorSevere("I can not add you now ! Redirecting in 3 sec...");
				echo mysql_error();
				header("Refresh: 3, url=$base_url./eeta.php");

			}
			else {
				echo "I have successfully added you to my database.";
			}
		}
	}
}

?>
