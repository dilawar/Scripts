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
		foreach($info as $key => $value) 
		{
			if($value == "xx") {
				return false;
			}
		}
		return true;
	}
}

function printStudentInfo($info) {
	
	$course = $info['specialization'];
	if($course == "xx") {
		$spec = "Not Given ";
		$_SESSION['completeInfo'] = "no";
	}
	else if($course == "ee1") {
		$spec = "Communication ";
	}
	else if($course == "ee2") {
		$spec = "Control and Computing ";
	}
	else if($course == "ee3") {
		$spec = "Power Electronics and System ";
	}
	else if($course == "ee4") {
		$spec = "Microelectronics and VLSI ";
	}
	else {
		$spec = "Electronic Systems ";
	}

	$program = $info['program'];
	switch($program) 
	{
	case "xx" :
		$prog = "Not given ";
		$_SESSION['completeInfo'] = "no";
		break;
	case "rs" :
		$prog  = "Research Scholar ";
		break;
	case "mtech" :
		$prog = "Master of Technology ";
		break;
	case "dd" :
		$prog = "Dual Degree (B.Tech + M.Tech.) ";
		break;
	} 

	$category = $info['category'];
	switch($category) 
	{
	case "xx" :
		$cat = "Not given ";
		$_SESSION['completeInfo'] = "no";
		break;
	case "ta" :
		$cat = "Teaching Assistant";
		break;
	case "ira" :
		$cat = "Institute Research Assistant ";
		break;
	case "pra" :
		$cat = "Project Research Assistant ";
		break;
	case "sf":
		$cat = "Self Financed ";
		break;
	case "qip" :
		$cat = "Qualility Imporvement Program ";
		break;
	case "sponsored" :
		$cat = "Sponsored ";
		break;
	}

	$gradOn = $info['graduatingOn']; // from database.

	if(!$gradOn) // from form and not from database.
	{
		$gradY = $info['gradYear'];
		$gradS = $info['gradSem'];
		if($gradS == "Odd") {
			$gradOn = "January 1 ".$gradY;
		}
		else {
			$gradOn = "August 1 ".$gradY;
		}
		$gradOn = date("Y-m-d", strtotime($gradOn));
	}

	$str = "<table border='1'>";
	$str .=  "<tr><td> LDAP Id </td> <td> <b>".$info['ldap']."</b></td></tr>";
	$str .= "<tr><td> Roll No </td> <td> <b>".$info['roll']."</b></td></tr>";
	$str .= "<tr><td> Program </td> <td> <b>".$prog."</b></td></tr>";
	$str .= "<tr><td> Category </td> <td> <b>".$cat."</b></td></tr>";
	$str .= "<tr><td> Specialization </td> <td> <b>".$spec."</b></td></tr>";
	$str .= "<tr><td> Graduating on </td> <td> <b>".$gradOn."</b></td></tr>";
	$str .= "</table>";
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

function getPreviousSem($sem, $count) 
{
	if($count == 0) {
		return $sem;
	}
	$count =  abs($count);
	$year = substr($sem, 0, 4); // first 4 character are year.
	$type = substr($sem, 4); // 4 to last index are sem.
	$quot = floor($count / 2);
	$rem = (int) ($count % 2);
	$newsem = "";
	$newyear = $year - $quot;
	if($rem == 0) {
		$newsem = $type;
	}
	else {
		if(strcmp($type, "Even") == 0)
		{
			$newsem = "Odd";
		}
		else {
			$newsem = "Even";
		}
	}
	return  $newyear.$newsem;

}

function generateSelect($name, $courses) {
	$html = "<select name=".$name.">";
	foreach($courses as $id => $cname) {
		$html .= "<option value=\"$cname[0]\"".">".$cname[0]." : ".$cname[2]." : ".$cname[1]."</option>";
	}
	$html .= "</select>";
	return $html;
}

function printSem($sem) {
	$year = substr($sem, 0, 4); // first 4 character are year.
	$type = substr($sem, 4); // 4 to last index are sem.

	return $type." semester, ".$year;
}

?>
