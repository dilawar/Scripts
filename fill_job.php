<?php include('header.php'); ?>
<?php
session_start();
include('sql_func.php');
include('error.php');

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];
$pSem = getPreviousSem($this_sem, 1);
$ppSem = getPreviousSem($this_sem, 2);

$pCourse = $_POST[$pSem]; 
$ppCourse = $_POST[$ppSem]; 

$ldap = $_SESSION['ldap'];
$ip = $init['db_ip'];
$pass = $init['db_pass'];
$user = $init['db_user'];
$init = $_SESSION['init'];

$res1 = updateHistory($pSem, $pCourse);
$res2 = updateHistory($ppSem, $ppCourse);

/*
$con = mysql_connect($ip, $user, $pass);
if(!$con) {
	echo printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
	$base_url = $init['base_url'];
	$url = "http://".$base_url."/preference.php";
	header("Refresh: 3, url=$url");
	exit;
}
$res = mysql_select_db("ta".$pSem, $con);
if(!$res) 
{
	echo printErrorSevere("Failed query with error ".mysql_error());
	$base_url = $init['base_url'];
	$url = "http://".$base_url."/preference.php";
	header("Refresh: 3, url=$url");
	exit;
}

$query = sprintf("insert into ta_record (ldap, semester, course_id) values ('%s', '%s', '%s')"
					, mysql_real_escape_string($ldap)
					, mysql_real_escape_string($pSem)
					, mysql_real_escape_string($pCourse)
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

$res = mysql_select_db("ta".$ppSem, $con);
if(!$res) 
{
	echo printErrorSevere("Failed query with error ".mysql_error());
	$base_url = $init['base_url'];
	$url = "http://".$base_url."/preference.php";
	header("Refresh: 3, url=$url");
	exit;
}

$query = sprintf("insert into ta_record (ldap, semester, course_id) values ('%s', '%s', '%s')"
					, mysql_real_escape_string($ldap)
					, mysql_real_escape_string($ppSem)
					, mysql_real_escape_string($ppCourse)
				);
$res = mysql_query($query, $con);

 */
if(!$res1 or !$res2) 
{
	echo printErrorSevere("Failed query with error ".mysql_error());
	$base_url = $init['base_url'];
	$url = "http://".$base_url."/preference.php";
	//header("Refresh: 3, url=$url");
	exit;
}
else 
{
	echo "Updated successfully.";
	$base_url = $init['base_url'];
	$url = "http://".$base_url."/preference.php";
	header("Refresh: 3, url=$url");
	exit;
}


?>
