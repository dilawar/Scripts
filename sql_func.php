<?php
session_start();

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];
$ldap = $_SESSION['ldap'];

function getCourseNameFaculty($id) 
{
	$init = $_SESSION['init'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) 
	{
		return null;
	}
	else 
	{
		$res = mysql_select_db("eecourses", $con);
		$query = sprintf("select name, faculty from courses where id='%s'"
			, mysql_real_escape_string($id)
		);
		$res = mysql_query($query, $con);
		$result = mysql_fetch_assoc($res);
		mysql_free_result($res);
		return $result;
	}
}


function getCourseList($db) 
{
	$init = $_SESSION['init'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) 
	{
		return null;
	}

	else {
		$res = mysql_select_db($db, $con);
		$query = sprintf("select id, name, faculty from course where running='%s'"
			, mysql_real_escape_string("yes"));
		$res = mysql_query($query, $con);
		$course_list = array();
		while($row = mysql_fetch_array($res))
		{
			array_push($course_list, $row);
		}
		mysql_free_result($res);
		return $course_list;
	}
}

?>
