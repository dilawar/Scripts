<?php include('header.php'); ?>
<?php
session_start();
include('func.php');
include('error.php');
include('sql_func.php');

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];
$ldap = $_SESSION['ldap'];

/*
 * Push preferences into database.
 */
$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
if(!$con) 
{
	echo printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
	echo mysql_error();
	header("Refresh: 3, url=$base_url./eeta.php");
}
else 
{
	$sem = $this_sem;
	$res = pushPreferences($sem, $_POST);
	if(pushPreferences($sem, $_POST)) 
	{
		$url = "http://".$init['base_url']."/final.php";
		header("Location: $url");
	}
	else 
	{
		echo printErrorSevere("Failed to update database with error");
		echo mysql_error();
		echo "<br> Redirecting in 5 second ..";
		$url = "http://".$init['base_url']."/eeta.php";
		header("Refresh: 5, url=$url");
	}
}

?>
