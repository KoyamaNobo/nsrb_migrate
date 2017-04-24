<?php
//header("Content-type: text/html; charset=SJIS");
//header("Cache-Control: Private");
?>
<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/DataView.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>DataView</title>
<script type="text/javascript" src="../js/jquery-1.12.4.min.js"></script>
<script type="text/javascript" src="./js/DataView_common.js?var=20160613"></script>
<script type="text/javascript" src="./js/DataView_free_search_2.js?var=20160613"></script>
<script type="text/javascript" src="./js/DataView_backButton.js?var=20160613"></script>
</head>
<body>
	<div class="contentbody">
		<div class="hblock">
			DataView
		</div>
		<div class="mblock">
<?php
require_once('./view/vw_Part_DataView_LoginInfo.php');
require_once('./view/vw_Part_DataView_ConstMessage.php');
require_once('./view/vw_Part_DataView_Menu.php');
?>

			<div class="main">
				<div class="navigate">
					<span class="bold">���R����</span>
					<div class="description">
						����������ݒ肵�Ă��������B<br/>
						�e�[�u���I����<span class="red">���������ݒ�</span>���\�����ڑI�������o�����ݒ聄���בւ��ݒ聄���ʕ\��
					</div>
				</div>


				<div class="summary">

<?php
require_once('./view/vw_Part_DataView_summaryTables.php');
?>

				</div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<form action="./DataView_free_search_3.php" method="post" enctype="multipart/form-data">
					<div class="transition">
						<p class="backButton DataView_free_search_1 center">�߂�</p>
						<p class="nextButton center">����</p>
					</div>
					<div class="contents">
						<div id="contentsTop" class="contentsTop">
							<div id="lcontents" class="lcontents lFreeSearchTable">
								<div class="leftListDetail">
									<div class="leftListDetailleft">
										<span>�e�[�u���P</span>
										<select id="lTableName" class="selectTableName" >
											<option value=""></option>
<?php
if(!empty($selectedTables)){
	foreach($selectedTables as $selectedTable){
?>
											<option value=<?php echo '"'.$selectedTable.'"' ?>><?php echo $selectedTable ?></option>
<?php
	}
}
?>
										</select>
										<div>���ꗗ</div>
									</div>
									<div id="lCandidate_Add" class="lCandidate_Add"></div>
								</div>
								<div id="leftListHeader"  class="leftListHeader">
									<div class="listHead">
										<span class="listItemName">���ږ�</span><span
										class="listJapaneseName">�a��</span><span
										class="listS_point center">�J�n�ʒu</span><span
										class="listSize center">�T�C�Y</span>
									</div>
								</div>
								<div>
									<div id="leftList"  class="leftList">
									</div>
								</div>
								<input type=hidden id="lStartNum" value="0">
							</div>
							<div id="ccontents" class="ccontents">
							</div>
							<div id="rcontents" class="rcontents rFreeSearchTable">
								<div class="rightListDetail">
									<div class="rightListDetailleft">
										<span>�e�[�u���Q</span>
										<select id="rTableName" class="selectTableName">
											<option value=""></option>
<?php
if(!empty($selectedTables)){
	foreach($selectedTables as $selectedTable){
?>
											<option value=<?php echo '"'.$selectedTable.'"' ?>><?php echo $selectedTable ?></option>
<?php
	}
}
?>
										</select>
										<div>���ꗗ</div>
									</div>
									<div id="rCandidate_Add" class="rCandidate_Add"></div>
								</div>
								<div id="rightListHeader" class="rightListHeader">
									<div class="listHead">
										<span class="listItemName">���ږ�</span><span
										class="listJapaneseName">�a��</span><span
										class="listS_point center">�J�n�ʒu</span><span
										class="listSize center">�T�C�Y</span>
									</div>
								</div>
								<div>
									<div id="rightList" class="rightList">
									</div>
								</div>
								<input type=hidden id="rStartNum" value="0">
							</div>
						</div>
						<div class="bcontents">
							<div class="divCenter">
								<div id="bondBox" class="radioBox">
									<input type="radio" id="bond1" name="bond" value="0" checked="checked"><label for="bond1">���̃e�[�u�������Ɍ���</label><br/>
									<input type="radio" id="bond2" name="bond" value="1"><label for="bond2">�݂��Ɉ�v�������̂�����</label><br/>
								</div>
							</div>
							<div class="divCenter center free_search_join">
								<img src="./img/btntbn04-3.png" id="bondAddButton" alt="����" width="" height="">
							</div>
							<div class="divCenter">
								<div>��������</div>
								<div id="bondListHeader"  class="bondListHeader">
									<div class="listHead bondListHead">
										<span class="lListTableName">�e�[�u����</span><span
										class="lListItemName">���ږ�</span><span
										class="listBond">��������</span><span
										class="rListTableName">�e�[�u����</span><span
										class="rListItemName">���ږ�</span>
									</div>
								</div>
								<div>
									<div id="bondList" class="bondList">

<?php
$oddFlg = -1;
if(!empty($selectedBonds)){
	foreach($selectedBonds as $selectedBond){
?>
										<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
											<span class="hidden bgChItems"></span><span
											class="lListTableName bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedBond['lListS_point']); ?>&#x0A;�T�C�Y : <?php echo htmlEscape($selectedBond['lListSize']); ?>&#x0A;�^ : <?php echo htmlEscape($selectedBond['lListType']); ?>"><?php echo htmlEscape($selectedBond['lListTableName']); ?></span><span
											class="lListItemName bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedBond['lListS_point']); ?>&#x0A;�T�C�Y : <?php echo htmlEscape($selectedBond['lListSize']); ?>&#x0A;�^ : <?php echo htmlEscape($selectedBond['lListType']); ?>"><?php echo htmlEscape($selectedBond['lListItemName']); ?></span><span
											class="hidden lListJapaneseName  bgChItems"><?php echo htmlEscape($selectedBond['lListJapaneseName']); ?></span><span
											class="hidden lListS_point  bgChItems"><?php echo htmlEscape($selectedBond['lListS_point']); ?></span><span
											class="hidden lListSize  bgChItems"><?php echo htmlEscape($selectedBond['lListSize']); ?></span><span
											class="hidden lListType  bgChItems"><?php echo htmlEscape($selectedBond['lListType']); ?></span><span
											class="listBond bgChItems"><?php echo htmlEscape($selectedBond['bondConditions']); ?></span><span
											class="rListTableName bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedBond['rListS_point']); ?>&#x0A;�T�C�Y : <?php echo htmlEscape($selectedBond['rListSize']); ?>&#x0A;�^ : <?php echo htmlEscape($selectedBond['rListType']); ?>"><?php echo htmlEscape($selectedBond['rListTableName']); ?></span><span
											class="rListItemName bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedBond['rListS_point']); ?>&#x0A;�T�C�Y : <?php echo htmlEscape($selectedBond['rListSize']); ?>&#x0A;�^ : <?php echo htmlEscape($selectedBond['rListType']); ?>"><?php echo htmlEscape($selectedBond['rListItemName']); ?></span><span
											class="hidden rListJapaneseName  bgChItems"><?php echo htmlEscape($selectedBond['rListJapaneseName']); ?></span><span
											class="hidden rListS_point  bgChItems"><?php echo htmlEscape($selectedBond['rListS_point']); ?></span><span
											class="hidden rListSize  bgChItems"><?php echo htmlEscape($selectedBond['rListSize']); ?></span><span
											class="hidden rListType  bgChItems"><?php echo htmlEscape($selectedBond['rListType']); ?></span><span
											class="hidden key bgChItems"><?php echo htmlEscape($selectedBond['key']); ?></span><span
											class="bondChildLast hidden id bgChItems"><?php echo htmlEscape($selectedBond['id']); ?></span></div>
<?php
	$oddFlg *= -1;
	}
}
?>

									</div>
									<img src="./img/btntbn06-2.png" id="bondDelButton" alt="�ナ�X�g�I���s���폜" width="" height="">
								</div>
							</div>
						</div>
					</div>
					<div class="transition">
						<p class="backButton DataView_free_search_1 center">�߂�</p>
						<p class="nextButton center">����</p>
					</div>
				</form>
			</div>
		</div>
	</div>
</body>
</html>
