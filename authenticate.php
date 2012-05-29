<html>
<body>
<h1> EE IITB Teaching Assistant Interface </h1>
<br>
<?php 

$proxy_user=$_REQUEST["username"];
$proxy_pass=$_REQUEST["pass"];
$acad_sem=$_REQUEST["year"].$_REQUEST["sem"];
$db_name="ta".$acad_sem;
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
		echo "Authentication successful! \n";
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
		echo "<font size=\"5\" color=\"blue\"> It is embarrasing but I can not connect to database! Redirecting in 3 seconds...</font>\n";
		header("Refresh: 3, url=$base_url./eeta.php");
	}
	else {

		# check if entry for the username already exists.
		mysql_query("CREATE DATABASE IF NOT EXISTS $db_name", $con);
		mysql_select_db("ta2012", $con);
		$sql= "CREATE TABLE IF NOT EXISTS job_table
			( LDAP varchar(20)
			, FirstName varchar(20)
			, LastName varchar(20)
			, WhichSem int
			, FirstChoice varchar(6)
			, SecondChoice varchar(6)
			, ThirdChoice varchar(6)
			, prevJob varchar(6)
			, pprevJob varchar(6)
			, ppprevJob varchar(6)
			, PRIMARY KEY(LDAP)
		)";
		mysql_query($sql, $con);
		mysql_query("use job_table");

	}
}
else {
		echo "<font size=\"4\" color=\"red\"> Failed to authenticate with error code : ".$httpCode['http_code']."</font><br>";
		echo "<br>font size=\"5\" color=\"blue\"> Redirecting in 3 seconds...</font>\n";
		header("Refresh: 3, url=$base_url./eeta.php");
}

?>

<?php 
echo $course_list;
$fh = fopen($course_list, "r");
if(!$fh) {
		echo "<font size=\"5\" color=\"blue\"> I can not find a course list! Redirecting in 3 seconds...</font>\n";
	header("Refresh: 3, url=$base_url./eeta.php");
}
$course_array = array();
while(! feof($fh))
{
	$data=fgetcsv($fh, 100, ",");
	array_push($course_array, $data);
}
if(!course_array) {
	echo "<br>Error in reading course list. Redirecting in 3 second...<br>";
	header("Refresh: 3, url=$base_url./eeta.php");
}
$courses = $course_array;
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
<?php echo generateSelect("ta1", $courses);	?>
Second Preference :
<?php echo generateSelect("ta2", $courses);	?>
Third Preference :
<?php echo generateSelect("ta3", $courses);	?>
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
