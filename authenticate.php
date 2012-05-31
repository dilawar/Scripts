<!-- This authenticate user with proxy-server 
(c) dilwar@ee.iitb.ac.in
-->
<?php include('header.php'); ?>

<?php 
session_start();
include('student.php');
include('teacher.php');
include('error.php');
include('func.php');

$init = $_SESSION['init'];
$proxy_user=$_REQUEST["username"];
$proxy_pass=$_REQUEST["pass"];
$acad_sem=$_REQUEST["year"].$_REQUEST["sem"];

if(strlen($proxy_user) < 2) {
	$proxy_user=getenv('proxy_username');
  $proxy_pass=getenv('proxy_password');
}

$_SESSION['ldap'] = $proxy_user;
$_SESSION['sem'] = $acad_sem;

if(strcmp($_REQUEST['Role'], "Teacher") == 0) {
	echo printErrorSevere("Interface to teachers is not available. Going back 5 sec...");
	header("Refresh: 5, url=$base_url./eeta.php");
}

$proxy_url = $init['proxy_url'];
$proxy_port = $init['proxy_port'];
$url = "http://www.google.com";

$res = authenticate(array($url, $proxy_url, $proxy_port, $proxy_user, $proxy_pass));

# if authentication is successful.
if($res) {
	$init = $_SESSION['init'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) {
		echo printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
		echo mysql_error();
		header("Refresh: 3, url=$base_url./eeta.php");
	}
	else {
		# check if entry for the username already exists.
		$res = mysql_select_db("ta".$acad_sem, $con);
		if(!$res) {
			echo printErrorSevere("I can not locate database for this semseter. Failed with ".mysql_error());
			echo mysql_error();
			header("Refresh: 3, url=$base_url./eeta.php");
		}
	}
	$res = mysql_select_db("eestudents", $con);
	if(!$res)
	{
		echo printErrorSevere("I can not communicate with database! Redirecting...");
		echo mysql_error();
		header("Refresh: 3, url=$base_url./eeta.php");
	}
	else {
		$query = sprintf("select * from student where ldap='%s'", 		
											mysql_real_escape_string($_SESSION['ldap']));
		$res = mysql_query($query, $con);
		if(!$res) {
			printErrorSevere("I can not fetch your information.");
		}
		else {
			$details = mysql_fetch_assoc($res);
			/* Print details and check if they are complete. Also provide edit button. 
			 */
				if(!checkStudentDetails($details))
				{
					$complete_info = false;
					echo printErrorSevere("Your details are not complete or missing. ");
					echo printStudentInfo($details);
?>
				<br>
				<form method="post" action="get_info.php">
					<input type="submit" name="response" value="Edit">
				</form>
				<br>
<?php

				}
				else 
				{
					session_write_close();
					echo "<b> Your details in my database </b> <br> <br>";
					echo printStudentInfo($details);

					### Ok or edit.
?>
				<br>
				<form method="post" action="get_info.php">
					<input type="submit" name="response" value="Edit">
					<input type="submit" name="response" value="O.K.">
				</form>
				<br>
<?php
			}
		}
	}
}

# can not authenticate.
else {
		echo printErrorSevere("Failed to authenticate at proxy-server! Redirecting in 5 sec ...");
		header("Refresh: 5, url=$base_url./eeta.php");
}

?>

<!--
<?php 
## This function converts course list into select options.
?>

-->

