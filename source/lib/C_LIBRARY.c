/* header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libcob.h>
#include "confheader.h"

#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN]="";
int MT_Initialize();
#endif


#define min(a,b) ((a) < (b) ? (a) : (b))

/* 文字列をcobolに転写する関数 */
void C_move_to_cob(char *cob_dat, const char *dat)
{
    int len = strlen(cob_dat);                      // data length in cob
    memset(cob_dat, ' ', min(len, strlen(dat)));    // clear with spaces
    memcpy(cob_dat, dat, min(len, strlen(dat)));    // data copy
    return;
}

///* 置換する。 buf の中の dlm を ato にする。成功=0 失敗=1 */
//int Replace(char *buf, char *dlm, char *ato)
//{
//    char *mituke;
//    size_t maelen, atolen;
//    
//    maelen = strlen(dlm);
//    atolen = strlen(ato);
//    if (maelen == 0 || (mituke = strstr(buf, dlm)) == NULL) return 1;
//    memmove(mituke + atolen, mituke + maelen, strlen(buf) - (mituke + maelen - buf ) + 1);
//    memcpy(mituke, ato, atolen);
//    return 0;
//}


//// SELECT句の装置指定およびボリューム名、ボリュ ーム通し番号
//// またはメディア識別名を、実行時に変更します。
//int PRMASGN(char *arg1,char *arg2,char *arg3)
//{
//	int ret = 0;
//	
//	return ret;
//}
//
//// ユーザの定義した変換表にしたがって、与えられたデータをコード変換します。
//// 引数１：変換対象／変換結果
//// 引数２：桁数
//// 引数３：変換表
//int CBLCODE(char *arg1,char *arg2,char *arg3)
//{
//	int ret = 0;
//	
//	return ret;
//}

// 可変パラメータで、ＰＭあるいはＪＳを起動します。
// 引数１：変換対象／変換結果
// 引数２：桁数(未使用)
int CBLRUN(char *arg1,char *arg2){
	int ret = 0,i = 0;
	char *str;
	char command[2048]="";
	
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," Error C [%02d]: CBLRUN is not match Argument count \n",99);
	
	if(cob_call_params != 2){
		fprintf(stderr,map_source_func);
		exit(1);
	}
	i = 0;
	memcpy(command,arg1,cob_current_module->cob_procedure_parameters[0]->size);
	
	//スペースに置換する文字
	char dlm1[] = "#_/>;";
	char ato = ' ';
	char split = 0x27;
	
	// 文字列から"#"を" "に置換
	str = command;
	while(strchr(str,(int) *(dlm1 + 0)) != NULL){
		str = strchr(str,(int) *(dlm1 + 0));
		*str = ato;
	}
	
	// 文字列から"_"を" "に置換
	str = command;
	while(strchr(str,(int) *(dlm1 + 1)) != NULL){
		str = strchr(str,(int) *(dlm1 + 1));
		*str = ato;
	}
	
	// 文字列から"/"を" "に置換
	str = command;
	while(strchr(str,(int) *(dlm1 + 2)) != NULL){
		str = strchr(str,(int) *(dlm1 + 2));
		*str = ato;
	}
	
	// 文字列から">"を"\0"に置換
	str = command;
	while(strchr(str,(int) *(dlm1 + 3)) != NULL){
		str = strchr(str,(int) *(dlm1 + 3));
		*str = split;
	}
	
//	
//	str = command;
//	while(strchr(str,(int) *(dlm1 + 4)) != NULL){
//		str = strchr(str,(int) *(dlm1 + 4));
//		*str = split;
//	}
	
	// 文字列";"の前にスペースを入れる ";"は"'"に置換
	str = command;
	if(strchr(str,(int) *(dlm1 + 4)) != NULL){
		char tmpP = ' ';  //ひとつ前を格納
		char tmpN = ' ';  //次を格納
		str = (char *)strchr(str,(int) *(dlm1 + 4));
		while(tmpN != '\0'){
			if(*str == *(dlm1 + 4)){
				tmpP = *str;
				*str = ' ';
			}else{
				tmpN = tmpP;
				tmpP = *str;
				
				if(tmpN == *(dlm1 + 4)){
					*str = split;
				}else{
					*str = tmpN;
				}
			}
			
			str++;
		}
	
		// コマンド実行
		char debugcmd[2048]="";
		sprintf(debugcmd,"logger -i \"%s\"",command);
		system(debugcmd);
		ret = system(command);
	}else{
		ret = 1;
	}
	
	//
	if((ret != 0)){
		ret = 1;
	}else{
		ret = 0;
	}
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	
	return ret;
}


// プログラムが動作しているワークステーションの、ワークステーション名を通知します。
int CBLSTNNO(char *arg1,char *arg2){
	int ret = 0;
	
	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," Error C [%02d]: CBLSTNNO is not match Argument count \n",99);
	
	if(cob_call_params != 2){
		fprintf(stderr,map_source_func);
		exit(1);
	}
	
	
	C_move_to_cob(arg1,arg2);
	
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	
	return ret;
}

// 仮クローズ（ファイルラベル更新） 不必要
int _CBLTCLS(char *arg1){
	int ret = 0;
	
	return ret;
}

// インターバルタイマの機能があります。指定された時間(0.1秒単位)だけ、時間待ちを行います。
int CBLTIMER(char *msc){
	int ret = 0;
	int time = atoi(msc) * 100000;
	
	usleep(time);
	
	return ret;
}