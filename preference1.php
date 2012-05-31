<?php include('header.php'); ?>
<?php
session_start();
include('func.php');
include('error.php');

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];
/*
 * Once reached here, ask for hostory and preference.
*/
$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
if(!$con) 
{
	echo printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
	echo mysql_error();
	header("Refresh: 3, url=$base_url./eeta.php");
}
else {
	$prevSem = getPreviousSem($this_sem, 1);
	$pprevSem = getPreviousSem($this_sem, 2);
	$ldap = $_SESSION['ldap'];

	/* check if this person has entry in database in previous semester. */
	$res = mysql_select_db("ta".$prevSem, $con);
	if(!$res) {
		echo "Query failed with error ".mysql_error();
	}
	$query = sprintf("select course_id from ta_record where ldap='%s' and semester='%s'"
			, mysql_real_escape_string($ldap)
			, mysql_real_escape_string($prevSem)
		);
	
	$res = mysql_query($query, $con);
	$pCourse = mysql_fetch_assoc($res);
	mysql_free_result($res);
	
	/* for prev prev semester. */
	$res = mysql_select_db("ta".$pprevSem, $con);
	if(!$res) {
		echo printErrorSevere("Failed query with error ".mysql_error());
		$base_url = $init['base_url'];
		$url = "http://".$base_url."/preference.php";
		header("Refresh: 3, url=$url");
		exit;
}
	$query = sprintf("select course_id from ta_record where ldap='%s' and semester='%s'"
			, mysql_real_escape_string($ldap)
			, mysql_real_escape_string($pprevSem)
		);

	$res = mysql_query($query, $con);
	if(!$res) 
	{
		echo printErrorSevere("Failed query with error ".mysql_error());
		$base_url = $init['base_url'];
		$url = "http://".$base_url."/preference.php";
		header("Refresh: 3, url=$url");
		exit;
	}
	$ppCourse = mysql_fetch_assoc($res);
	mysql_free_result($res);
	
}
?>
