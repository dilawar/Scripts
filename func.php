<?php 
session_start();

function getStudentDetails($name, $con) {
			return "success";
}

function checkStudentDetails($info) 
{
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
	foreach($info as $key => $value) {
		$str .= $key.":".$value."<br>";
	}
	return $str;
}

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


?>
