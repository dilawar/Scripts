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

/* check if edit or Ok. */
if(strcmp($_POST['response'], "Edit") == 0)
{
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) 
	{
		echo printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
		echo mysql_error();
		header("Refresh: 3, url=$base_url./preference.php");
	}
	else 
	{
		$sem = $this_sem;
		$course_list = getCourseList($sem);
?>
<html>
<h3> Three preferences for this semester </h3>
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
<form action="get_preference.php" method="post">
First choice
<?php echo generateSelect("first", $course_list);	?> <br /> <br />
Second choice
<?php echo generateSelect("second", $course_list);	?> <br /> <br />
Third choice
<?php echo generateSelect("third", $course_list);	?> <br /> <br />
<input type="submit" name="Submit" value="Submit" />
</form>
</body>
</html>
<?php
	
	}
}
else 
{
	$url = "http://".$init['base_url']."/final.php";
	header("Location: $url");
}
?>
