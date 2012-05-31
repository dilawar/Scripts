<?php include('header.php') ?>
<h3> Let us know what you did last year </h3>
<font color="red"> Please be careful! Once submitted, you can not change your these values. </font>
<?php
session_start();
include('sql_func.php');

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];

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

	mysql_select_db("ta".$prevSem, $con);
	if(!$con) {
		echo printErrorSevere("Failed query with error ".mysql_error());
		$base_url = $init['base_url'];
		$url = "http://".$base_url."/preference.php";
		header("Refresh: 3, url=$url");
		exit;
	}
	$query = sprintf("select id, name, faculty from course where running='%s'"
		, mysql_real_escape_string("yes"));
	$res = mysql_query($query, $con);
	$course_list_1 = array();
	while($row = mysql_fetch_array($res))
	{
		array_push($course_list_1, $row);
	}
	// Now create a form.
	mysql_select_db("ta".$pprevSem, $con);
	if(!$con) {
		echo printErrorSevere("Failed query with error ".mysql_error());
		$base_url = $init['base_url'];
		$url = "http://".$base_url."/preference.php";
		header("Refresh: 3, url=$url");
		exit;
	}
	$query = sprintf("select id, name, faculty from course where running='%s'"
		, mysql_real_escape_string("yes"));
	$res = mysql_query($query, $con);
	$course_list_2 = array();
	while($row = mysql_fetch_array($res))
	{
		array_push($course_list_2, $row);
	}
?>
<html>
<head>
<style type="text/css">
	.container{
		width : 200px;
		clear : both;
	}
	.container input {
		width : 30%;
		clear : both;
}
</style>
</head>
<body>
<div class="container">
<form action="fill_job.php" method="post">
<?php echo printSem($prevSem) ?>
<?php echo generateSelect($prevSem, $course_list_1);	?> <br /> <br />
<?php echo printSem($pprevSem) ?>
<?php echo generateSelect($pprevSem, $course_list_2);	?> <br /> <br />
<input type="submit" name="Submit" value="Submit" />
</form>
</body>
</html>
<?php
}

?>
