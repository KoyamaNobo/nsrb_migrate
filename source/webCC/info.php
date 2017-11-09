<?php 
if (!preg_match('/192\.168\.*/',$_SERVER['REMOTE_ADDR'])) die(); 
?>
<?php phpinfo(); ?>