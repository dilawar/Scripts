<html>
<body>
<h1> EE IITB Teaching Assistant Interface </h1>
<br>
<?php 
include('student.php');
include('teacher.php');
include('error.php');

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
	$location=$_SERVER['PATH_INFO'];
	$sqlpass=$_SERVER['sql_pass'];
	if(strlen($sqlpass) < 2) {
		$sqlpass="dilawar123";
	}
	$con = mysql_connect("10.107.105.13", "dilawar", $sqlpass);
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
}
else {
		echo printErrorSevere("Failed to authenticate at proxy-server! Redirecting in 5 sec ...");
		header("Refresh: 5, url=$base_url./eeta.php");
		exit(0);
}

?>

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
		$html .= "<option value=".$cname[0].">".$cname[0].": ".$cname[1]."</option>";
	}
	$html .= "</select>";
	return $html;
}
?>
