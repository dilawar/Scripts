<?php 

function printErrorSevere($msg) {
	$err = "<font size=\"4\" color=\"blue\">".$msg."<br>";
	return $err;
}

function sendEmailToAdmin($err_msg, $db_name) {

}

function printWarning($msg) {
	$warn ="<font size=\"3\" color=\"blue\">".$msg."<br>";
	return $warn;
}
?>
