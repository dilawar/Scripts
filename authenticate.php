<html>
<body>
<h1> EE IITB Teaching Assistant Interface </h1>
<br>
<?php 

$proxy_user=$_REQUEST["username"];
$proxy_pass=$_REQUEST["pass"];
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
		echo "I can not authenticate you with IIT Proxy server. \n";
		echo "Error code : ".$httpCode['http_code']."\n";
		echo "Redirecting in 3 seconds...\n";
		header("Refresh: 3, url=$base_url./eeta.php");
		return false;
	}
}

$res = authenticate(array($url, $proxy_url, $proxy_port, $proxy_user, $proxy_pass));

# if authentication is successful.
if($res) {
	$location=$_SERVER['PATH_INFO'];
	$sqlpass=$_SERVER['sql_pass'];
	if(strlen($sqlpass) < 2) {
		$sqlpass="rashmirathi";
	}
	$con = mysql_connect("10.107.105.13", "root", $sqlpass);
	if(!$con) {
		echo "\nIt is embarrasing but I can not connect to database!\n Redirecting in 3 seconds...\n";
		header("Refresh: 3, url=$base_url./eeta.php");
	}
	else {
		# check if entry for the username already exists.
		mysql_query("CREATE DATABASE IF NOT EXISTS ta2012", $con);
		mysql_select_db("ta2012", $con);
		$sql= "CREATE TABLE IF NOT EXISTS tas 
			( LDAP varchar(20),
				FirstName varchar(20),
				LastName varchar(20),
				PRIMARY KEY(LDAP))";
		mysql_query($sql, $con);
		mysql_close($con);

	}
}

?>

