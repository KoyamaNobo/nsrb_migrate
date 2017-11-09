<?php

$file = $_GET["name"];

if(isset($_FILES) && isset($_FILES["file"]) && isset($_FILES["file"]["error"]) && $_FILES["file"]["error"] == 0 )
{
    //ファイルが存在しているか
    if (file_exists($file)) {
        //ファイルが開けるか
        if (!is_writable($file)) {
             echo('ERR9008');
             exit;
        }
    }

    //ファイルをコピーしてファイル作成
    if (move_uploaded_file($_FILES['file']['tmp_name'], $file)) {
        //chmod( $file, 0666 );
    } else {
        echo('ERR9009');
        exit;
    }
}
else
{
    echo("ERR9010");
    exit;
}
?>
