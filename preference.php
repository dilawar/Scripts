<?php include('header.php'); ?>
<?php
session_start();
include('error.php');
include('sql_func.php');
include('print.php');

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];
$ldap = $_SESSION['ldap'];
$missingHistory = false;
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
	$missingHistory = false;
	$prevSem = getPreviousSem($this_sem, 1);
	$pprevSem = getPreviousSem($this_sem, 2);

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
	$pCourse = mysql_fetch_array($res);
	mysql_free_result($res);

	if(!$pCourse) 
	{
		$missingHistory = true;
	}
	/* for prev prev semester. */
	$res = mysql_select_db("ta".$pprevSem, $con);
	if(!$res) {
		echo "Query failed with error ".mysql_error();
	}
	$query = sprintf("select course_id from ta_record where ldap='%s' and semester='%s'"
			, mysql_real_escape_string($ldap)
			, mysql_real_escape_string($pprevSem)
		);
	$res = mysql_query($query, $con);
	if(!$res) 
	{
		echo "Query failed with error ".mysql_error();
	}
	$ppCourse = mysql_fetch_array($res);
	mysql_free_result($res);

	if(!$ppCourse) 
	{
		$missingHistory = true;
	}
	if($missingHistory)
	{
		$url = "http://".$init['base_url']."/history.php";
		header("Location: $url");
	}

	else {
?>
<h3> Your history of previous two semester. </h3> 
<small>Send an email to administrator to change them.</small>
<table border="1">
<tr> <td> <?php echo printSem($prevSem); ?> </td> <td> <b> <?php echo $pCourse['course_id']; ?> </b> </td> </tr>
<tr> <td> <?php echo printSem($pprevSem); ?> </td> <td> <b> <?php echo $ppCourse['course_id']; ?> </b> </td> </tr>
</table>
<br> <br>
<?php
		// Get course list for this semester.
		mysql_select_db("ta".$this_sem, $con);
		if(!$con) {
			echo printErrorSevere("Failed query with error ".mysql_error());
			$base_url = $init['base_url'];
			$url = "http://".$base_url."/eeta.php";
			header("Refresh: 3, url=$url");
		}

		/* check if ta-job is alloted. */
		$query = sprintf("select course_id from ta_record where ldap='%s'"
				, mysql_real_escape_string($ldap)
			);
		$res = mysql_query($query, $con);
		$ta_job = mysql_fetch_assoc($res);
		mysql_free_result($res);
		if($ta_job)  /* job is alloted to you. */
		{
			echo "A job has been alloted to you.<br>";
			$id = $ta_job['course_id'];
			$course = getCourseNameFaculty($id);
			echo "Course name : <b>".$course['name']."</b>";
			echo "Faculty : <b>".$course['faculty']."</b>";
			exit;
		}

		/* 
		 * Check if preferences are already submitted. 
		 */
		$preferences = getPreferences($this_sem);
		if($preferences) 
		{
			echo "<h4> Your preferences with us are  </h4>";
			echo printPreference($preferences);

?>
	<br>
	<form action="edit_preference.php" method="post">
	<input type="submit" name="response" value="Edit" />
	<input type="submit" name="response" value="O.K." />
	</form>
	<br>
<?php
		}
		else 
		{
			$course_list = getCourseList($this_sem);
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
}

?>
