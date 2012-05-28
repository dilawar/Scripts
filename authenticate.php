<html>
<body>
<h1> EE IITB Teaching Assistant Interface </h1>
<br>
<?php 
echo "Curl";

$proxy_user=$_REQUEST["username"];
$proxy_pass=$_REQUEST["pass"];
$proxy_url = "netmon.iitb.ac.in:80";

$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "http://google.com/");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_PROXY, $proxy_url);
curl_setopt($ch, CURLOPT_PROXYUSERPWD, $porxy_user.";".$proxy_pass);
culr_setopt($ch, CURLOPT_PROXYPORT, 80);
culr_setopt($ch, CURLOPT_PROXYAUTH, CURAUTH_NTLM);

$headers['Authorization'] = 'Basic '. base64_encode("$proxy_user:$proxy_pass");

$file = curl_exec($ch);
echo $file;

?>

</body>
</html>
