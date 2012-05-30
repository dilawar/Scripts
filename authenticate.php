<!-- This authenticate user with proxy-server 
(c) dilwar@ee.iitb.ac.in
-->

<h1> EE IITB Teaching Assistant Interface </h1>

<?php 

include('student.php');
include('teacher.php');
include('error.php');
include('func.php');

$HOME="/pg/rs/dilawar";
session_save_path($HOME."/sessions/");
if(session_start())
{
	echo "Sesson start successfully.";
}
else {
	echo "Problem loading session.";
}

$inifile = "$HOME"."/sessions/eeta.ini";
if(!file_exists($inifile)) {
	printErrorSevere("Init file does not exists.");
	header("No configuration file found. Application incomplete ..");
}
$conf = parse_ini_file($inifile);
if(!$conf)
{
	header("No configuration file found. Application incomplete ..");
}
else {
	$_SESSION['init'] = $conf;
}
$proxy_user=$_REQUEST["username"];
$proxy_pass=$_REQUEST["pass"];
$acad_sem=$_REQUEST["year"].$_REQUEST["sem"];
$db_name="ta".$acad_sem;
$db_course="courses".$acad_sem;
$course_list="./courses/course_".$acad_sem.".txt";
$base_url="http://www.ee.iitb.ac.in/student/~dilawar/Scripts/";

if(strlen($proxy_user) < 2) {
	$proxy_user=getenv('proxy_username');
  $proxy_pass=getenv('proxy_password');
}

$_SESSION['ldap'] = $proxy_user;
$_SESSION['acad_sem'] = $acad_sem;
$_SESSION['db_name'] = $db_name;
$_SESSION['db_course'] = $db_course;

if($_REQUEST['Role'] == "Teacher") {
	echo printErrorSevere("Not implemented. Redirecting in 5 sec...");
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
		printErrorSevere("It is embarrasing but I can not connect to database! Redirecting in 3 sec...");
		echo mysql_error();
		header("Refresh: 3, url=$base_url./eeta.php");
	}
	else {
		# check if entry for the username already exists.
		$res = mysql_select_db($_SESSION['db_name'], $con);
		if(!$res) {
			printErrorSevere("Can not locate database for this semseter.".mysql_error()."An 
				email is sent to adminstrator");
			echo mysql_error();
			header("Refresh: 5, url=$base_url./eeta.php");
		}
	}
	$res = mysql_select_db("eestudents", $con);
	if(!$res)
	{
		printErrorSevere("I can not communicate with database! Redirecting...");
		echo mysql_error();
		header("Refresh: 5, url=$base_url./eeta.php");
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
			echo "<b> We have your details : </b> <br>";
			echo printStudentInfo($details);
		}
	}

	/*
	if(checkStudentDetails($student_info))
	{
		echo "Details are ok.";
		echo printStudentInfo($student_info);
		$complete_info = true;
	}
	else 
	{
		//ob_start();
		session_write_close();
		echo "<br>Your details with us are following : <br>";
		echo printStudentInfo($student_info);
		//header("Location: $base_url/get_info.php");
		//$output = ob_get_clean();
	}
	 */
}

# can not authenticate.
else {
		echo printErrorSevere("Failed to authenticate at proxy-server! Redirecting in 5 sec ...");
		header("Refresh: 5, url=$base_url./eeta.php");
		exit(0);
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

