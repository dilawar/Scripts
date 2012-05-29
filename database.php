<h1>EE IITB Teaching Assistant Interface</h1>

<?php
include('error.php');
session_save_path(getenv('HOME'."/sessions"));
session_start();

$ldap = $_SESSION['user_ldap'];
$ip = $_SESSION['sql_ip'];
$pass = $_SESSION['sql_pass'];

$con = mysql_connect($ip, "dilawar", $pass);
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
	$_gradSem = $_POST['gradSem'];
	$_graduatingOn = "";
	if($_gradSem == "even") {
		$_graduatingOn = "1 September ".$_gradYear;
	}
	else {
		$_graduatingOn = "1 January ".$_gradYear;
	}
	$_graduatingOn = date("Y-m-d", strtotime($_graduatingOn));	
	
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
		echo "Unsuccesful with error ".mysql_error();
	}
	else {
		header("Location: ".$_SESSION['base_url']."/preference.php");
	}
}

?>
