:!<html>
<body>
<h1> EE IITB Teaching Assistant Interface </h1>
<br>
<?php 
echo "Curl";

$proxy_user=$_REQUEST["username"];
$proxy_pass=$_REQUEST["pass"];
$proxy_url = "netmon.iitb.ac.in:80";
$proxy_port = 80;
$url = "http://www.google.com";

$headers = array("HTTP/1.1",
        "Content-Type: application/x-www-form-urlencoded",
        "Cache-Control: no-cache",
        "Authorization: Basic " . base64_encode($proxy_user.":".$proxy_pass)
    );

$ch = curl_init(); 
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_HEADER, $headers);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_USERAGENT, $defined_vars['HTTP_USER_AGENT']);
curl_setopt($ch, CURLOPT_PROXYPORT, $proxy_port);
curl_setopt($ch, CURLOPT_PROXYTYPE, 'HTTP');
curl_setopt($ch, CURLOPT_PROXY, $proxy_url);
curl_setopt($ch, CURLOPT_PROXYUSERPWD, $proxy_user.":".$proxy_pass);
$data = curl_exec($ch);
echo $data;

?>

</body>
</html>
