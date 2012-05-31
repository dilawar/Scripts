<?php
session_start();
include('func.php');

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
		$query = sprintf("select id, name, faculty from courses where id='%s'"
			, mysql_real_escape_string($id)
		);
		$res = mysql_query($query, $con);
		$result = mysql_fetch_assoc($res);
		mysql_free_result($res);
		return $result;
	}
}


function getCourseList($sem) 
{
	$db = "ta".$sem;
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

function getPreferences($sem) 
{
	$db = "ta".$sem;
	$init = $_SESSION['init'];
	$ldap = $_SESSION['ldap'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) 
	{
		return null;
	}

	else 
	{
		$res = mysql_select_db($db, $con);
		$query = sprintf("select first, second, third from preference where ldap='%s'"
			, mysql_real_escape_string($ldap)
		);
		$res = mysql_query($query, $con);
		$preferences = mysql_fetch_assoc($res);
		return $preferences;
	}
}

function pushPreferences($sem, $post) 
{
	$db = "ta".$sem;
	$init = $_SESSION['init'];
	$ldap = $_SESSION['ldap'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) 
	{
		return null;
	}

	else 
	{
		$first = $post['first'];
		$second = $post['second'];
		$third = $post['third'];
		/* if entry is already present then update else insert. */
		if(getPreferences($sem))
		{
			$res = mysql_select_db($db, $con);
			$query = sprintf("update preference set first='%s', second='%s'
				, third='%s' where ldap='%s'"
				, mysql_real_escape_string($first)
				, mysql_real_escape_string($second)
				, mysql_real_escape_string($third)
				, mysql_real_escape_string($ldap)
				);
			$res = mysql_query($query, $con);
			return $res;
		}
		else 
		{
			$res = mysql_select_db($db, $con);
			$query = sprintf("insert into preference set first='%s', second='%s'
				, third='%s', ldap='%s'"
				, mysql_real_escape_string($first)
				, mysql_real_escape_string($second)
				, mysql_real_escape_string($third)
				, mysql_real_escape_string($ldap)
				);
			$res = mysql_query($query, $con);
			return $res;
		}
	}
}

function getStudentInformation($sem)
{
	$init = $_SESSION['init'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) {
		return null;
	}
	else {
		# check if entry for the username already exists.
		$res = mysql_select_db("ta".$sem, $con);
		if(!$res) {
			return null;
		}
	}
	$res = mysql_select_db("eestudents", $con);
	if(!$res)
	{
		return null;
	}
	else {
		$query = sprintf("select * from student where ldap='%s'", 		
											mysql_real_escape_string($_SESSION['ldap']));
		$res = mysql_query($query, $con);
		if(!$res) {
			return null;
		}
		else {
			$details = mysql_fetch_assoc($res);
			return $details;
		}
	}
}

function getHistory($sem, $count)
{

	$init = $_SESSION['init'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) 
	{
		return null;
	}
	else {
		$prevSem = getPreviousSem($sem, 1);
		$pprevSem = getPreviousSem($sem, 2);

		$ldap = $_SESSION['ldap'];

		mysql_select_db("ta".$prevSem, $con);
		
		$query = sprintf("select course_id from ta_record where ldap='%s'"
			, mysql_real_escape_string($ldap));
		$res = mysql_query($query, $con);
		$course1 = mysql_fetch_array($res);
		mysql_free_result($res);

		mysql_select_db("ta".$pprevSem, $con);
		$query = sprintf("select course_id from ta_record where ldap='%s'"
			, mysql_real_escape_string($ldap));
		$res = mysql_query($query, $con);
		$course2 = mysql_fetch_array($res);
		mysql_free_result($res);	
		$history = array();
	
		array_push($history, $course1);
		array_push($history, $course2);

		return $history;
	}
}

function updateHistory($sem, $course)
{
	$init = $_SESSION['init'];
	$base_url = "http://".$init['base_url'];
	$this_sem = $_SESSION['sem'];
	$ldap = $_SESSION['ldap'];

	$init = $_SESSION['init'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) {
		return null;
	}
	
	$res = mysql_select_db("ta".$sem, $con);
	if(!$res) 
	{
		return null;
	}
	/* if there is history, then update it else insert. */
	if(!fetchHistory($sem))
	{
		$query = sprintf("insert into ta_record (ldap, semester, course_id) values ('%s', '%s', '%s')"
			, mysql_real_escape_string($ldap)
			, mysql_real_escape_string($sem)
			, mysql_real_escape_string($course)
		);

		$res = mysql_query($query, $con);
		return $res;
	}
	else /* there is an entry, update it. */
	{
		$query = sprintf("update ta_record set semester='%s', course_id='%s' where ldap='%s'"
			, mysql_real_escape_string($sem)
			, mysql_real_escape_string($course)
			, mysql_real_escape_string($ldap)
		);
		$res = mysql_query($query, $con);
		return $res;
	}
}

/* return course-id of ta-job done in particular semester. */
function fetchHistory($sem)
{
	$init = $_SESSION['init'];
	$base_url = "http://".$init['base_url'];
	$this_sem = $_SESSION['sem'];
	$ldap = $_SESSION['ldap'];

	$init = $_SESSION['init'];
	$con = mysql_connect($init['db_ip'], $init['db_user'], $init['db_pass']);
	if(!$con) {
		return null;
	}
	
	$res = mysql_select_db("ta".$sem);
	if(!$res) { 
		return null; 
	}

	$query = sprintf("select course_id from ta_record where ldap='%s'"
		, $ldap
	);
	$res = mysql_query($query, $con);
	$history = mysql_fetch_array($res);
	return $history;
}


	

?>
