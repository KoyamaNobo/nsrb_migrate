			<div class="box">
				<div class="menu">
					<ul type="disc">
<?php
$url = basename($_SERVER['PHP_SELF'],".php");
if (preg_match("/DataView_index(.)*/", $url)) {
	echo '<li name="DataView_menu" class="menuLink DataView_menu selected"><label>トップ</label></li>';
}else if(preg_match("/DataView_menu(.)*/", $url)){
	echo '<li name="DataView_menu" class="menuLink DataView_menu selected"><label>トップ</label></li>';
}else{
	echo '<li name="DataView_menu" class="menuLink DataView_menu"><label>トップ</label></li>';
}

if (preg_match("/DataView_command_search(.)*/", $url)) {
	echo '<li name="DataView_command_search" class="menuLink DataView_command_search selected"><label>条件タグ検索</label></li>';
}else if (preg_match("/DataView_disp(.)*/", $url) && preg_match("/DataView_command_search(.)*/", $prePgName)) {
	echo '<li name="DataView_command_search" class="menuLink DataView_command_search selected"><label>条件タグ検索</label></li>';
}else{
	echo '<li name="DataView_command_search" class="menuLink DataView_command_search"><label>条件タグ検索</label></li>';
}

if (preg_match("/DataView_free_search(.)*/", $url)) {
	echo '<li name="DataView_free_search_1"  class="menuLink DataView_free_search_1 selected"><label>自由検索</label></li>';
}else if (preg_match("/DataView_disp(.)*/", $url) && preg_match("/DataView_free_search(.)*/", $prePgName)) {
	echo '<li name="DataView_free_search_1" class="menuLink DataView_free_search_1 selected"><label>自由検索</label></li>';
}else{
	echo '<li name="DataView_free_search_1" class="menuLink DataView_free_search_1"><label>自由検索</label></li>';
}

if (preg_match("/DataView_item_conf(.)*/", $url)) {
	echo '<li name="DataView_item_conf" class="menuLink DataView_item_conf selected"><label>和名設定</label></li>';
}else{
	echo '<li name="DataView_item_conf" class="menuLink DataView_item_conf"><label>和名設定</label></li>';
}

if (preg_match("/users_item(.)*/", $url)) {
	echo '<li name="users_item" class="menuLink users_item selected"><label>項目設定</label></li>';
}else{
	echo '<li name="users_item" class="menuLink users_item"><label>項目設定</label></li>';
}

if (preg_match("/DataView_user_conf(.)*/", $url)) {
	echo '<li name="DataView_user_conf" class="menuLink DataView_user_conf selected"><label>ユーザ設定</label></li>';
}else{
	echo '<li name="DataView_user_conf" class="menuLink DataView_user_conf"><label>ユーザ設定</label></li>';
}

echo '<li name="DataView_logout" class="menuLink DataView_logout"><LABEL>ログアウト</LABEL></li>';
?>

					</ul>
				</div>
			</div>
