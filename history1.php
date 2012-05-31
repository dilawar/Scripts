<?php include('header.php') ?>
<h3> Job done in previous semesters. </h3>
<?php
session_start();
session_start();
include('func.php');

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
	$ldap = $_SESSION['ldap'];

	/* check if this person has entry in database in previous semester. */
	mysql_select_db("ta".$prevSem, $con);
	if(!$con) {
		echo "Query failed with error ".mysql_error();
	}
	$query = sprintf("select id, name, faculty from course where running='%s'"
		, mysql_real_escape_string("yes"));
	mysql_select_db("ta".$prevSem);
	$res = mysql_query($query, $con);
	$course_list = array();
	while($row = mysql_fetch_array($res))
	{
		array_push($course_list, $row);
	}
	// Now create a form.
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
<h3><?php echo printSem($prevSem); ?> </h3>
<form action="fill_job.php" method="post">
Course :
<?php echo generateSelect("ta1", $course_list);	?>
<input type="submit" name="Submit" value="Submit" />
</form>
</body>
</html>
<?php
}

?>
