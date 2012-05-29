<!-- This authenticate user with proxy-server 
(c) dilwar@ee.iitb.ac.in
-->
<html>
<body>
<h1> EE IITB Teaching Assistant Interface </h1>
<br>
<?php 
session_save_path(getenv('HOME'."/sessions"));
session_start();
$proxy_user=$_REQUEST["username"];
$proxy_pass=$_REQUEST["pass"];
$acad_sem=$_REQUEST["year"].$_REQUEST["sem"];
$db_name="ta".$acad_sem;
$db_course="courses".$acad_sem;
$course_list="./courses/course_".$acad_sem.".txt";
$base_url="http://www.ee.iitb.ac.in/student/~dilawar/Scripts/";
$history_exists = false;
$complete_info = false;
$db_ip = "10.107.105.13";

include('student.php');
include('teacher.php');
include('error.php');


if(strlen($proxy_user) < 2) {
	$proxy_user=getenv('proxy_username');
  $proxy_pass=getenv('proxy_password');
}
$_SESSION['user_ldap'] = $proxy_user;
$_SESSION['acad_sem'] = $acad_sem;
$_SESSION['db_name'] = $db_name;
$_SESSION['db_course'] = $db_course;
$_SESSION['base_url'] = $base_url;
$_SESSION['sql_ip'] = $db_ip;
$_SESSION['sql_pass'] = "dilawar123";

if($_REQUEST['Role'] == "Teacher") {
	echo printErrorSevere("Not implemented. Redirecting in 5 sec...");
	header("Refresh: 5, url=$base_url./eeta.php");
	exit(0);
}

$proxy_url = "netmon.iitb.ac.in:80";
$proxy_port = 80;
$url = "http://www.google.com";

# function to authenticate user with proxy-server.
function authenticate($input) {
	$headers = array("HTTP/1.1",
					"Content-Type: application/x-www-form-urlencoded",
					"Cache-Control: no-cache",
					"Authorization: Basic " . base64_encode($input[3].":".$input[4])
			);

	$ch = curl_init(); 
	curl_setopt($ch, CURLOPT_URL, $input[0]);
	curl_setopt($ch, CURLOPT_HEADER, $headers);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
	curl_setopt($ch, CURLOPT_USERAGENT, $defined_vars['HTTP_USER_AGENT']);
	curl_setopt($ch, CURLOPT_PROXYTYPE, 'HTTP');
	curl_setopt($ch, CURLOPT_PROXY, $input[1]);
	curl_setopt($ch, CURLOPT_PROXYPORT, $input[2]);
	curl_setopt($ch, CURLOPT_PROXYUSERPWD, $input[3].":".$input[4]);
	$data = curl_exec($ch);
	$httpCode = curl_getinfo($ch);
	if($httpCode['http_code']=='302') {
		return true;
	}
	else {
				return false;
	}
}

$res = authenticate(array($url, $proxy_url, $proxy_port, $proxy_user, $proxy_pass));

# if authentication is successful.
if($res) {
	echo "Here I am";
	$sqlpass=$_SERVER['sql_pass'];
	if(strlen($sqlpass) < 2) {
		$sqlpass="dilawar123";
	}
	$con = mysql_connect($db_ip, "dilawar", $sqlpass);
	if(!$con) {
		echo printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
		header("Refresh: 3, url=$base_url./eeta.php");
	}
	else {
		# check if entry for the username already exists.
		$res = mysql_select_db($db_name, $con);
		if(!$res) {
			echo printErrorSevere("Can not locate database for this semseter.".mysql_error()."An email is sent to adminstrator");
			header("Refresh: 5, url=$base_url./eeta.php");
			sendEmailToAdmin("database_connect_error".$mysql_error(), $db_name);
			exit(0);
		}
	}
	$_SESSION['sql_con'] = $con;
	$student_info = getStudentDetails($proxy_user, $con);
	if(checkStudentDetails($student_info))
	{
		echo "Details are ok.";
		echo printStudentInfo($student_info);
		$complete_info = true;
	}
	else 
	{
		echo "Here I am";
		//ob_start();
		session_write_close();
		header("Location: $base_url/get_info.php");
		//$output = ob_get_clean();
		echo $output;
	}
}

# can not authenticate.
else {
		echo printErrorSevere("Failed to authenticate at proxy-server! Redirecting in 5 sec ...");
		header("Refresh: 5, url=$base_url./eeta.php");
		exit(0);
}
?>

<?php 
function getStudentDetails($name, $con) {
	if(mysql_select_db("eestudents", $con) == NULL) {
		printErrorSevere("I can not communicate with database! Redirecting...");
		header("Refresh: 5, url=$base_url./eeta.php");
		exit(0);
	}
	else {
		$res = mysql_query("select * from students where ldap=$name", $con);
		$details = mysql_fetch_assoc($res);
		return $details;
	}
}

function checkStudentDetails($info) 
{
	echo "Info";
	if(!$info) {
		return false;
	}
	else {
		foreach($info as $value) 
		{
			if(strlen($value) < 1) {
				return false;
			}
		}
		return true;
	}
}

function printStudentInfo($info) {
	$str = "<br>Your details with us are following : <br>";
	foreach($info as $value) {
		$str .= print_r($value);
	}
	return $str;
}

?>

<!--
<?php
## Find courses from database. 
$res = mysql_select_db($db_course, $con);
if(!$res) {
	echo printErrorSevere("Not courses found for this semester ... An email is sent to admin.");
	header("Refresh: 5, url=$base_url./eeta.php");
	sendEmailToAdmin("course_list_error".$mysql_error(), $db_course);
	exit(0);
}
else {
	$course = mysql_query("select * from courses");
	if(!$course) {
		echo printErrorSevere("Error reading database ...");
		exit(0);
	}
	else {
		$course_array = array(array());
		while($row = mysql_fetch_assoc($course)) {
			$this_course = array($row['id'], $row['name'], $row['faculty']);
			array_push($course_array, $this_course);
		}
	}
}
?>


<?php 
# if last two semester histroy is not available in database, ask
# for it.
if(!$historyExists) {
	echo printWaring("We do not have your past reacord. Please fill details. Redirecting you to appropriate page ...");

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
<h3>Job description</h3>
<form action="database.php" method="post">
First Preference :
<?php echo generateSelect("ta1", $course_array);	?>
Second Preference :
<?php echo generateSelect("ta2", $course_array);	?>
Third Preference :
<?php echo generateSelect("ta3", $course_array);	?>
<input type="submit" name="Submit" value="Submit" />
</form>
</body>
</html>

<?php 
## This function converts course list into select options.
function generateSelect($name, $courses) {
	$html = "<select name=".$name.">";
	foreach($courses as $id => $cname) {
		$html .= "<option value=".$cname[0].">".$cname[0]." : ".$cname[2]." : ".$cname[1]."</option>";
	}
	$html .= "</select>";
	return $html;
}
?>

-->
