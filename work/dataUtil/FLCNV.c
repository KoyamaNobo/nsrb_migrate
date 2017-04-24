#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <mysql.h>
#include <time.h>
#include <signal.h>
#include <libxml/xmlreader.h>    /*conf�t�@�C����xml��ǂނ���*/
#include <libxml/xpath.h>        /*conf�t�@�C����xml��ǂނ���*/

// container_of �}�N��
#define container_of(ptr, type, member) ({                              \
        const typeof( ((type *)0)->member ) *__mptr = (void *)(ptr);    \
        (type *)( (char *)__mptr - offsetof(type,member) );})

/*
#define container_of(ptr, type, member) ({                              \
		(type *)( (char *)(ptr) - offsetof(type,member) );})
*/

//DB�̃^�O��
#ifndef CONF_DB_TAG
#define CONF_DB_TAG 1
const char* CONF_DB_HOST = "dbHost";
const char* CONF_DB_USER = "dbUser";
const char* CONF_DB_PASS = "dbPass";
const char* CONF_DB_NAME = "dbName";
const char* CONF_DB_PORT = "dbPort";
const char* MANA_TBL_NAME = "dbTMName";
const char* DEBUG_FLGNAME = "debug";
#endif
#ifndef CONF_PATH
#define CONF_PATH
char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";            //20150915���[�J���� conf.xml ���Ȃ��Ƃ��̑Ή�
char* CONF_DEFPATH  = "//conf/*/text()";    //�g�b�v���x������꒼����\��
#endif
#define OPER_LEN (4)		// ���Z�q�̕�����
#define ITEMLST 256		// SORT�Ώۍ\���̗̂v�f��
#define SQLSTR (2048)		// SQL������̍ő咷
#define MAP_SRC_FUNC_LEN 50
#define PATHNAME_SIZE 1024
#define MODECHLEN 3

#define mysql_failure() __mysql_failure(__func__, __LINE__)

//�萔
#define WK_INTSTR_LEN 11
const int  NSHIFT     = 4;            // �����J�n��������p�̈ړ�����
const int  ARGITM     = 5;            // �����\���̗̂v�f��
//const int  ITEMLST    = 20;         // SORT�Ώۍ\���̗̂v�f��
const char *EXCLUSIVE = "EXCLUSIVE";  //2015/08/28 kawada add �I�[�v�����[�h�F�r��(EXCLUSIVE)
const char *PROTECT   = "PROTECT";    //2015/08/28 kawada add �I�[�v�����[�h�F�ی�(PROTECT)
const char *SHARE     = "SHARE";      //2015/08/28 kawada add �I�[�v�����[�h�F���p(SHARE)

//DB�ݒ�\����
struct db_setting {
	char db_server[64];        // host��
	char db_user[64];          // user��
	char db_password[64];      // password
	char db_database[64];      // database��
	int db_port;               // port�ԍ�
	char db_manatbl[64];       // �Ǘ�table��    //2015/08/28 kawada add �Ǘ��e�[�u����
}db_setting={"","","","",3306};
typedef struct db_setting DB_SETTING;

//SORT�Ώۂ̈����\����
struct argv_inf {
	int  s_point;          // �J�n�ʒu
	int  length;           // ������
	char *type;            // �o�̓^�C�v
	char *operand;         // ��r�Ώۃt�B�[���h
	char *value;           // �o�̓t�B�[���h�\��
	int  logical;          // �_�����Z�q 0:AND,1:OR,-1:���̑�
//	char *opmod;           // DOS�`���̃t�@�C���I�[�v�����[�h Ex:EXCLUSIVE, Px:PROTECT, Sx:SHARE�ix�͕ϊ����@�ɂ��ς��j	//2015/08/28 kawada add

};
typedef struct argv_inf Argv_inf;

//SORT�Ώۍ\����
struct trgt_inf {
//	�e�����l�̍ő啶����
//	char ifi[12];              // ���̓t�@�C����
//	char ofi[12];              // �o�̓t�@�C����
//	char mod[03];              // �������ݕ��@
//	char out[120];             // �o�̓t�B�[���h�\��
//	char sel[120];             // where��
	char     *ifi;             // ���̓t�@�C����
	char     *ofi;             // �o�̓t�@�C����
	int      mod;              // �������ݕ��@ 0:ADD,1:NEW
	char     opmod[MODECHLEN]; // DOS�`���̃t�@�C���I�[�v�����[�h Ex:EXCLUSIVE, Px:PROTECT, Sx:SHARE�ix�͕ϊ����@�ɂ��ς��j
	Argv_inf out[ITEMLST];     // �o�̓t�B�[���h�\��
	Argv_inf sel[ITEMLST];     // where��
//	int      logical;          // �_�����Z�q 0:AND,1:OR,-1:���̑�
};
typedef struct trgt_inf Trgt_inf;

//������\����
struct string_t{
	size_t size;            // ������̒��� + NIL�����̒����̍��v�l
	char   *data;           // �f�[�^�̐擪
};
typedef struct string_t String_t;

///////////////////////////////////////////////Function liet AND prototype Start
//SORT�Ώۍ\���̂̏�����
void argv_ini(Argv_inf * );
//SORT�Ώۍ\���̂̏�����
void trgt_ini(Trgt_inf * );
//SORT�Ώۍ\���̂̉��
void trgt_free(Trgt_inf * );
// recieve INTERRUPT signal
void* sigInt( int  );
// recieve SegmentationFalet signal
void* sigSegv( int  );
// recieve Abort signal
void* sigAbrt( int  );
// recieve USER1 signal
void* sigUsr1( int  );
// recieve USER2 signal
void* sigUsr2( void* );
//�G���[�n���h����ݒ�
int setErrorHundle();
//RTrim����
int isNullOrEmpty(char *);
//���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ�
char *local_server_time(char*);
//�v���Z�X����Ԃ�
int getUserAndProcessName(char *,char *);
//�v���Z�X����Ԃ�
char *getprocessName(char *,int );
//--------String_t�֐��n--------
// �ϒ������� *ioData �� iStr �ŏ���������
int string_init(String_t *ioData, char const * iStr);
//�G���[�Ƀv���Z�X����t���ďo��
void mytool_runtime_error(char ,const char *sfile,const char *fmt, ...);
//�ݒ�t�@�C���̃p�X���擾
char *getConfFilename(char *);
//�ݒ�t�@�C���̓ǂݍ���
int conf_read(DB_SETTING *);
//�e�[�u�����̑O��́u`�v���������Ă���
char *convertTablenameToVariable(char *);
//--------�f�[�^�x�[�X�n--------
// ���j�[�NID����
int getUid();
//�����Ώۃt�@�C����LOCK����
int setTableLock(DB_SETTING *, Trgt_inf *,char *);
//�����Ώۃt�@�C����UNLOCK����
int setTableUnlock(DB_SETTING*, Trgt_inf*);
//�ݒ�̓ǂݍ��݂ƃf�[�^�x�[�X�ւ̐ڑ����s��
int db_init(DB_SETTING *);
//�O���[�o���ϐ�����ݒ���擾���A�f�[�^�x�[�X�ɐڑ�����
int DB_Open(DB_SETTING *);
//�ݒ�̒l���g���ăf�[�^�x�[�X�����
int DB_Close();
//SQL���s�G���[���̌㏈��
int __mysql_failure(const char *,int );
//SQL���s
int mysql_run(char * );
///////////////////////////////////////////////Function liet AND prototype End

///////////////////////////////////////////////Global Variables Start
int unqId = 0;            //���j�[�NID(�Ǘ��e�[�u���F�ؗp)		//20160506 add koyama�e�[�u���A�N�Z�X�̊Ǘ��ɔ���
char *myoptions=0;            //��ԍŌ�̂̃p�X���擾(�����ݒ�)

static char source_file_name[PATHNAME_SIZE];
static char source_user_name[PATHNAME_SIZE];

//conf����擾����debug_flg.���Ɣ��Ȃ��悤��original name
static int myConfDebugFlg;

MYSQL *mysqlConn;
MYSQL_RES *res;
MYSQL_ROW row;
char map_source_func[MAP_SRC_FUNC_LEN]="";

DB_SETTING *background_targSetting;
Trgt_inf   *background_itrgt;

///////////////////////////////////////////////Global Variables End

//*********************************************************
//SORT�Ώۍ\���̂̏�����
//*********************************************************
void argv_ini(Argv_inf *ioargv ) {
//	"�J�n�ʒu", "������", "�o�̓t�@�C����", "��r�Ώۃt�B�[���h", "�o�̓t�B�[���h�\��"
//	Argv_inf wk_argv ={0,0,"","",""};
	ioargv->s_point  =0;
	ioargv->length   =0;
	ioargv->type     =NULL;    //NULLPO�ŏ�����
	ioargv->operand  =NULL;    //NULLPO�ŏ�����
	ioargv->value    =NULL;    //NULLPO�ŏ�����
	ioargv->logical  =-1;
//	ioargv->opmod    =NULL;    //NULLPO�ŏ�����
//	*ioargv = wk_argv;
}


//*********************************************************
//SORT�Ώۍ\���̂̏�����
//*********************************************************
void trgt_ini(Trgt_inf *iotrgt ) {
	int ii = 0;
	//���������e
//	char     *wk_ifi = "";            // ���̓t�@�C����
//	char     *wk_ofi = "";            // �o�̓t�@�C����
//	int       wk_mod = 0;            // �������ݕ��@ 0:ADD,1:NEW
//	Argv_inf *wk_out[ITEMLST];            // �o�̓t�B�[���h�\��
//	Argv_inf *wk_sel[ITEMLST];            // where��
//	int       wk_logical = 0;            // �_�����Z�q 0:AND,1:OR,-1:���̑�

	Trgt_inf wktrgt;

	*iotrgt = wktrgt;

	iotrgt->ifi = 0;    //NULLPO�ŏ�����
	iotrgt->ofi = 0;    //NULLPO�ŏ�����
	iotrgt->mod = 1;
	memset(iotrgt->opmod,'\0',MODECHLEN);
//	iotrgt->opmod = (char *)malloc(sizeof(char) * 2);            //DOS�`���̃t�@�C���I�[�v�����[�h	//2015/08/28 kawada add

	for(ii = 0; ii < ITEMLST ; ii++){
		argv_ini(&iotrgt->out[ii]);
		argv_ini(&iotrgt->sel[ii]);
	}

//	iotrgt->logical = 0;

}

// recieve INTERRUPT signal
// Author koyama
void* sigInt( int args ){
	int i = 0;

	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve interrupt signal ");
	raise(SIGTERM);

	return NULL;
}

// recieve SegmentationFalet signal
// Author koyama
void* sigSegv( int args ){
	int i = 0;

	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	if (map_source_func && strlen(map_source_func) > 0) {
		mytool_runtime_error('E',source_file_name, " Error C [99]:recieve Segmentation Fault signal :%s %s ", map_source_func,myoptions);
	}else{
		mytool_runtime_error('E',source_file_name," Error C [99]:recieve Segmentation Fault signal  %s  ",myoptions);
	}
	raise(SIGTERM);

	return NULL;
}
// recieve Abort signal
// Author koyama
void* sigAbrt( int args ){
	int i = 0;
	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve Abort signal ");
	raise(SIGTERM);

	return NULL;
}


// recieve USER1 signal
// Author koyama
void* sigUsr1( int args ){
	int i = 0;

	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	mytool_runtime_error('E',source_file_name," Error C [99]:recieve usr1=kill signal ");
	raise(SIGTERM);

	return NULL;
}

// recieve USER2 signal
// Author koyama
void* sigUsr2( void *args ){
	int i = 0;

	//�G���[�o�͂����邱�Ƃ�Abort��ݒ�
	raise(SIGSTOP);

	return NULL;
}

//
//�G���[�n���h����ݒ�
//author : koyama
int setErrorHundle(){
	int retval;
	retval = 0;

	//signal
	//
	if (signal(SIGINT, (__sighandler_t)sigInt) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//KILL����
	if (signal(SIGUSR1, (__sighandler_t)sigUsr1) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//STOP����
	if (signal(SIGUSR2, (__sighandler_t)sigUsr2) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//SIGSEGV����
	if (signal(SIGSEGV, (__sighandler_t)sigSegv) == SIG_ERR){
		retval = 1;
		return retval;
	}
	//SIGABRT����
	if (signal(SIGABRT, (__sighandler_t)sigAbrt) == SIG_ERR){
		retval = 1;
		return retval;
	}
	return retval;

}

//RTrim����
//(���ꂼ���Rtim������̂ł����ł���p����������)
//date:20160520
//auth: koyama
int isNullOrEmpty(char *targ){
	char *strstart;
	char *strend;
	int num=0;
	strstart = targ;
	//�Ō�̕������画��Ώ�
	strend = targ + strlen(strstart);
	for(strend--;strend >= strstart;strend--){
		if(*strend == ' '){
			num++;
		}
	}
	//�S�Ă��X�y�[�X�Ȃ�
	if(num == strlen(strstart)){
		num = 1;
	}else{
		num = 0;
	}
	return num;
}

//
//���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ�
//in/out :retStr �Ԃ��Ώە�����̃|�C���^
//author : koyama
char *local_server_time(char *retStr){
	struct timeval timer;
	struct tm *date;

	gettimeofday(&timer, NULL);
	date = localtime(&timer.tv_sec);
	sprintf(retStr,"%02d/%02d/%02d %02d:%02d:%02d.%06d"
		,date->tm_year+1900
		,date->tm_mon+1
		,date->tm_mday
		,date->tm_hour
		,date->tm_min
		,date->tm_sec
		,timer.tv_usec);
	return retStr;
}

//�v���Z�X����Ԃ�
//���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ�
//in/out :setName �v���Z�X����Ԃ�������|�C���^ in/out:setUser ���[�U����Ԃ�������|�C���^ ::::������̊֐��͕�������I�[�o����ꍇ�𖳎�
//author : koyama
int getUserAndProcessName(char *setName,char *setUser){
	FILE  *fp;
	char  buf[1024];   //�R�}���h�̎󂯎��p
	int   pid=0;
	char  cmdReturn[2048]="";
	char  cmd[1024]="";
	char  *cmdReturnP;
	char  *cmdReturnNexrP;
	pid = getpid();
	sprintf(cmd,"ps -p %d -o cmd= ",pid);

	if ( (fp=popen(cmd,"r")) ==NULL) {
		//�R�}���h�̎��s�Ɏ��s������󕶎����return
		return 0;
	}
	//���ʂ�size�������R�s�[
	//����`�͕K�v�H
	while(fgets(buf, 1024, fp) != NULL) {
		if((strlen(cmdReturn) + strlen(buf) ) > 2048){
			//�R�}���h�̎��s�Ɏ��s������󕶎����return
			memset(cmdReturn,'\0',2048);
			return 0;
		}
		strcat(cmdReturn,buf);
	}
	pclose(fp);
	//�܂��ЂƂڂ̃X�y�[�X��T��
	cmdReturnP = strchr(cmdReturn,' ');
	if(cmdReturnP != NULL){
		strncpy(setName,cmdReturn,(cmdReturnP - cmdReturn));
		cmdReturnP = cmdReturnP + 1;
		cmdReturnNexrP = strchr(cmdReturnP,' ');
		if(cmdReturnNexrP != NULL){
			//Util��p�d�l
			if(strncmp(cmdReturnNexrP,"STN",strlen("STN")) == 0){
				strncpy(setUser,cmdReturnP,(cmdReturnNexrP - cmdReturnP));
			}
		}else{
			strncpy(setUser,cmdReturnP,strlen(cmdReturnP));
			//���s������\�������� ->����΍폜
			if(strchr(setUser, '\n') != NULL){
				*(strchr(setUser, '\n')) = '\0';
			}
		}
	}else{
		strncpy(setName,cmdReturn,strlen(cmdReturn));
	}

	return 1;
}


//�v���Z�X����Ԃ�
//in/out :setName �v���Z�X����Ԃ�������|�C���^ in:size �v���Z�X����Ԃ�������̒���
//author : koyama
char *getprocessName(char *setName,int size){
	FILE  *fp;
	char  buf[1024];   //�R�}���h�̎󂯎��p
	int   pid=0;
	char  cmd[1024]="";
	pid = getpid();
	sprintf(cmd,"ps -p %d -o comm= ",pid);

	memset(setName,'\0',size+1);
	if ( (fp=popen(cmd,"r")) ==NULL) {
		//�R�}���h�̎��s�Ɏ��s������󕶎����return
		return setName;
	}
	memset(setName,'\0',size+1);
	//���ʂ�size�������R�s�[
	//����`�͕K�v�H
	while(fgets(buf, 1024, fp) != NULL) {
		if((strlen(setName) + strlen(buf) ) > size){
			//�R�}���h�̎��s�Ɏ��s������󕶎����return
			memset(setName,'\0',size+1);
			return (char *)0;
		}
		strcat(setName,buf);
	}
	pclose(fp);
	//�ЂƂڂ̉��s�܂łŐ؂�
	if( strchr(setName, '\n') ){
		*(strchr(setName, '\n')) = '\0';
	}
	return setName;
}


//�����񒆂̎w�蕶���̐����J�E���g
//in :haystack �����Ώۂ̕�����,needle �w�蕶��
//out: retval�����񒆂Ɋ܂܂��w�蕶���̐�
//author : koyama
int searchChr(char *haystack ,char needle){
	int retval=0;
	char *pstr;
	pstr = haystack;
	while(*pstr != '\0'){
		if(*pstr == needle){
			retval += 1;
		}
		pstr++;
	}
	return retval;
}

//����̕�����u�����Ȃ���Ȃ���
//origText:�Ώە�����,target:�Ώە���
//,replacement:�u���㕶��,splitter����ň͂܂ꂽ����������B�w�肵�Ȃ��Ƃ���0
//return �Ȃ�
//author:koyama
void chgChar(char *origText,char target,char replacement,char splitter){
	char *origTextEnd;
	char *copyText;
	char elemflg = 0x00;
	char splitchar = 0x00;

	splitchar = splitter;
	if(splitchar == 0x00){
		//splitter�ɋ���w�肳�ꂽ�Ƃ��͔�΂��d�l
		elemflg = 0x01;
	}

	copyText  = origText;
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;copyText < origTextEnd;copyText++){
		//���s�������΂�
		if(splitchar == *copyText){
			if(elemflg == 0x00){
				elemflg = 0x01;
			}else{
				elemflg = 0x00;
			}
		}
		if(elemflg != 0x00 && *copyText == target){
			//������ɓ������Ƃ��͗]����+
			*copyText = replacement;

		}
	}
}


//����̕������������Ȃ���Ȃ���
//return �Ȃ�
void remTargChar(char *origText,char targ){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//���s�������΂�
		if(*copyText == targ ){
			//������ɓ������Ƃ��͗]����+
			copyText++;
		}
		//�����̃R�s�[
		*origText = *copyText;
		copyText++;
	}
}

//����̕������������Ȃ���Ȃ���
//return �Ȃ�
void remCRLF(char *origText){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//���s�������΂�
		if(*copyText == 0x0A || *copyText == 0x0D){
			//������ɓ������Ƃ��͗]����+
			copyText++;
		}
		//�����̃R�s�[
		*origText = *copyText;
		copyText++;
	}
}



//*********************************************************
//�X�v���b�g�֐�(�ꕶ���Ή�)
//*********************************************************
int strSplit__( char *iStr, char *iDelim, char *oList[] ) {
	char	*tk;
	int		cnt = 0;

	tk = strtok( iStr, iDelim );
	while( tk != NULL && cnt < 1000 ) {
		oList[cnt++] = tk;
		tk = strtok( NULL, iDelim );
	}
	return cnt;
}

//
//�X�v���b�g�֐�(��L�֐��ł͋󕶎���ɑΉ����Ȃ��̂ō��ς�)
//in/out :oList ��؂���������̔z��
//in     :�Ώە�����
//author : koyama
int strSplit( char *iStr, char *iDelim, char **oList ) {
	char	*tk;
	char    *preneedle,*needle,*postneedle;
	int		icnt = 0,ocnt=0;

	//token
	tk = iStr;

	//�ŏ���token
	oList[ocnt] = tk;
	ocnt++;

	//�ŏ���token�ŏ��������Ă���
	preneedle  = tk;
	needle     = tk;
	postneedle = tk;

	for(;preneedle != NULL && ocnt < 4096;ocnt++){
		//needle���Ȃ��Ȃ�܂�
		for(icnt = 0; *(iDelim + icnt) != '\0';icnt++){
			postneedle = strchr(preneedle,*(iDelim + icnt));
			//�����������A�����Ă���ꍇ�͔�΂�
			for(;postneedle !=NULL && *(postneedle + 1) == *(iDelim + icnt);){
				postneedle = (postneedle + 1);
			}
			if(icnt==0 || postneedle < needle){
				needle = postneedle;
			}
		}
		//�g�������Ƃ̂܂܂������甲����
		if(needle == NULL){
			oList[ocnt] = NULL;
			break;
		}
		//���݂�token���i�[
		oList[ocnt] = (needle + 1);
		*needle = '\0';
		preneedle  = (needle + 1);
		//�g�������Ƃ�needle��null��
		needle = NULL;
	}
	return ocnt;
}
//*********************************************************
//�X�v���b�g�֐�(�v�f�����u)�v����) &[@]<&[)]���O��
//*********************************************************
int strSplit_ELM( char *iStr, char *oList[] ) {
	char	*tk;
	int		cnt = 0;
	int		str_flg = 0;
	char	*str1;            //�u@�v�ʒu(�����ʒu�͔�r�����̐擪�|�C���^)
	char	*str2;            //�u)�v�ʒu(�����ʒu�͔�r�����̐擪�|�C���^)

	str1 = iStr;
	str2 = strstr(iStr, ")");

	//������v�f�u@�v�݂͂�����
	if(strstr(iStr, "@") != NULL){
		if(str2 < strstr(iStr, "@")){
			while(str1 > str2){
				if(strstr(strstr(iStr, "@"),"@") != NULL){
					str_flg = 1;
					str1 = strstr(strstr(iStr, "@"),"@");
				}
			}

		}
	}

	//��؂蕶��������
	if(str_flg == 1){
		while(str1 > str2){
			str2 = strstr(str2, ")");
		}
		tk = str2;
		oList[cnt++] = tk;
		tk = strtok( str2, ")" );
	}else{
		tk = strtok( iStr, ")" );
	}

	while( tk != NULL && isNullOrEmpty(tk)!=0 && cnt < 1000 ) {
		oList[cnt++] = tk;
		tk = strtok( NULL, ")" );
	}

	return cnt;
}

//*********************************************************
//������u��(�o�C�i���f�[�^��Ή�)
//add comment koyama string.h�̊֐����g���Ă��邽��'\0'(NULL)�ɑΉ����Ȃ�
//*********************************************************
int strChg(char *ioBuf, const char *iStr1, const char *iStr2)
{
	int		ret = 1;
	char	tmp[1024 + 1];
	char	*p;
	char	s = '\0';

	while ((p = strstr(ioBuf, iStr1)) != NULL) {
		//������Ȃ��Ȃ�܂ŌJ��Ԃ��Bp�͋�������̐擪���w��
		*p = s;            // ���̕��������������̒��O�ŋ�؂���
		p += strlen(iStr1);            // �|�C���^����������̎��̕�����
		strcpy(tmp, p);            // �������񂩂���ۑ�
		strcat(ioBuf, iStr2);            // �V����������̌�ɂȂ�
		strcat(ioBuf, tmp);            // ����Ɏc����Ȃ�
		ret = 0;            // �q�b�g�����ꍇ�ɖ߂�l�ύX
	}

	return ret;
}
//�����񂪂��ׂĐ�����
//ioTrgt:�Ώە�����
//return �G���[1,����0
//author:koyama
int isStrDigit(char *strTarg){
	int ret=0;
	char *temp;
	temp = strTarg;

	while(*temp != '\0'){
		if(isdigit(*temp) == 0){
			ret = 1;
			break;
		}
		temp++;
	}

	return ret;
}

//
//�Ώۂ̃I�v�V�����̗v�f���̃`�F�b�N
//input:�Ώۂ̃I�v�V��������������������|�C���^,�Ώۂ̃I�v�V�����̗v�f��
//author:koyama
int checkParamNum(char *strTarg,int intNum){
	int ret=0;
	char strTime[] = "0000/00/00 00:00:00.000000";

	//SEL
	if(strncmp(strTarg,"SEL",strlen("SEL")) == 0 && (intNum > 5)){
		fprintf(stderr," %s : Error C [%02d]:Too many arguments to %.3s                %s "
			,source_file_name,22,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}
	//OUT
	if(strncmp(strTarg,"OUT",strlen("OUT")) == 0 && intNum > 2){
		fprintf(stderr," %s : Error C [%02d]:Too many arguments to %.3s                %s \n"
			,source_file_name,23,strTarg,local_server_time(strTime));
		ret=1;
		exit(1);
	}

	return ret;
}


//
//���ʂ̑Ή���check
//input:�Ώە�����,�����L�[�𕶎���ɂ�������n,n+1�Ԗڂ��g�ݍ��킹�ƂȂ�
//�������n�������玟��nn+1������ΐ�,���Ȃ���Δ�
//n+1������O��nn��������ċA�I�Ɏ���nn+1��T��
//author:koyama
int checkParentheses(char *haystack,char *needles){
	if(needles == NULL){
		return 1;
	}
	if(haystack == NULL){
		return 1;
	}
	int targStrLen = strlen(haystack);
	int pairSize = strlen(needles) / 2;
	int nn=0,ii=0;
	int ret=0;

	//�����L�[�̐����[�v
	for(nn = 0; nn < pairSize;nn++){
		for(ii=0;ii < targStrLen;ii++){
			char *currS;
			char *currE;
			char *nextS;

			currS = strchr(haystack,needles[nn]);
			//nn���Ώۂɑ��݂��邩
			if(currS != NULL){
				//�I���̏ꏊ��
				currE = strchr((currS + 1),needles[nn + 1]);
				nextS = strchr((currS + 1),needles[nn]);
				if(currE == NULL){
					ret = 1;
				}

				//������ɊJ������������nest
				if(currE > nextS){
					ret = checkParentheses((currS + 1),(needles + nn));
				}
			}else{
				break;
			}
		}
	}
	return ret;
}

//--------String_t�֐��n--------�֐�Start
//*********************************************************
// �ϒ������� *ioData �� iStr �ŏ���������
//*********************************************************
int string_init(String_t *ioData, char const * iStr){
	int ret = 0;

	// size_t size = strlen(iStr);
	size_t size = 8192;

	ioData->data = (char *)malloc(sizeof(char) * (size + 1) );
	/* �m�ۂ����̈��NULL�N���A */
	ioData->size = 0;
	memset( ioData->data , '\0' , sizeof(char) * (size + 1));

	if(ioData->data != NULL){
		ioData->size = strlen(iStr);
		memcpy(ioData->data , iStr, ioData->size);
	}else{
		ret = 1;
	}

	return ret;
}

//*********************************************************
// �ϒ������� *ioData ��j������
//*********************************************************
int string_fin(String_t *ioData){
	memset(ioData->data,'\0',strlen(ioData->data));
	free(ioData->data);
	return 0;
}

//*********************************************************
// �ϒ�������̒�����Ԃ�
//*********************************************************
size_t string_length(char ** ioData){
	return (container_of(*ioData, String_t, data))->size - 1;
}

//*********************************************************
// �A������
//*********************************************************
int string_concat(String_t * ioData, char *iStr){
	int ret = 0;

  if(iStr==NULL){
    //�Ȃ��ϐ���NULL�Ȃ牽�����Ȃ�
    return ret;
  }
	size_t size = strlen(iStr) + ioData->size;

	/* �m�ۂ����̈��NULL�N���A */
	if(ioData->data != NULL){
		//���ɕ��������̂܂܌q���̂�NULL�͌q���Ȃ�
		memcpy(ioData->data + ioData->size, iStr, strlen(iStr));
		ioData->size = size;
	}else{
		ret = 1;
	}

	return ret;
}

//*********************************************************
// ������ String �� nShift �����ړ�����B
// �ړ���ւ̃|�C���^��Ԃ��B
//*********************************************************
char *StrShift( char *String, size_t nShift ){
	char *start = String;
	char *stop  = String + strlen( String );
	memmove( start + nShift, start, stop-start+1 );

	return String + nShift;
}

//*********************************************************
// ������ String �̕����� From �𕶎��� To �Œu������B
// �u����̕����� String �̃T�C�Y�� String �̋L���̈�𒴂���ꍇ�̓���͖���`�B
//*********************************************************
char *StrReplace( char *String, const char *From, const char *To ){
	int   nToLen;   // �u�����镶����̒���
	int   nFromLen; // �������镶����̒���
	int   nShift;
	char *start;    // �������J�n����ʒu
	char *stop;     // ������ String �̏I�[
	char *p;

	nToLen   = strlen( To );
	nFromLen = strlen( From );
	nShift   = nToLen - nFromLen;
	start    = String;
	stop     = String + strlen( String );

	// ������ String �̐擪���當���� From ������
	while( NULL != ( p = strstr( start, From ) ) )
	{
		// ������ To �����ʂł���悤�ɂ���
		start = StrShift( p + nFromLen, nShift );
		stop  = stop + nShift;

		// ������ To �𕡎�
		memmove( p, To, nToLen );
	}

	return String;
}//StrReplace


//*********************************************************
//�v�f�����֐�
//*********************************************************
int set_argv(Argv_inf *ioTrgt_arg, char * iArgv){
	int ii,jj, ret = 0;

	char *strOptArr[ITEMLST] = {'\0'};        //�I�v�V������
	char *strArgArr[ARGITM + 1];    //�v�f��
	int result_ii = 0;
	int result_jj = ARGITM;
	char strTime[] = "0000/00/00 00:00:00.000000";

	//���ʂ̑Ή���check
	if(checkParentheses(iArgv,"()")){
		fprintf(stderr," Error C [%02d]:Parentheses is not supported              %s \n",10,local_server_time(strTime));
		exit(1);
	}

	if(isNullOrEmpty(iArgv)){
		return 0;
	}

	remTargChar(iArgv,0x5c);
	remCRLF(iArgv);
	result_ii = strSplit_ELM(iArgv,strOptArr);

	if(result_ii >= ITEMLST){
		fprintf(stderr," Error C [%02d]:Too many arguments                        %s \n",16,local_server_time(strTime));
		exit(1);
	}

	for(ii=0; ii< result_ii; ii++){
		//�s�v�Ȋ���,A/O�̍폜
		if(ii > 0){
			//AND/OR�����̏ꍇ(��؂蕶���̐擪��A/O)
			if(*strOptArr[ii] == 'A'){
				ioTrgt_arg[ii].logical = 0;
			}else if(*strOptArr[ii] == 'O'){
				ioTrgt_arg[ii].logical = 1;
			}else{
				if(ii==0 || *strOptArr[ii] == ','){
					ioTrgt_arg[ii].logical = -1;
				}else{
					fprintf(stderr," Error C [%02d]:comma can't be found                %s \n",12,local_server_time(strTime));
					exit(1);
				}
			}
			if(strchr(strOptArr[ii],'(') != NULL){
				strOptArr[ii] = strchr(strOptArr[ii],'(');
			}
		}

		strChg(strOptArr[ii],"(","");
		strChg(strOptArr[ii],")","");

		result_jj = strSplit(strOptArr[ii],",",strArgArr);

		//�����̐��𔻒�
		//�G���[�̎��͊֐�����exit
		checkParamNum(strOptArr[0],result_jj);

		//�ŏ��̗v�f�ƌ���string�̃|�C���^�ʒu�͓����͂�
		if(strArgArr[0] != strOptArr[ii]){
			fprintf(stderr," Error C [%02d]:param parse error                       %s \n",11,local_server_time(strTime));
			exit(1);
		}

		if((strlen(strOptArr[ii]) > 0 && result_jj == 0) || result_jj > ARGITM){
			fprintf(stderr," Error C [%02d]:Can't determine specific element        %s \n",13,local_server_time(strTime));
			exit(1);
		}

		for(jj=0; jj< result_jj; jj++){
			switch(jj){
			case 0:
				//������
				ioTrgt_arg[ii].s_point  = 0;            // �J�n�ʒu
				ioTrgt_arg[ii].length   = 0;            // ������
				ioTrgt_arg[ii].type     = "";            // �o�̓^�C�v
				ioTrgt_arg[ii].operand  = "";            // ��r�Ώۃt�B�[���h
				ioTrgt_arg[ii].value    = "";            // �o�̓t�B�[���h�\��

				if (result_jj == 1){
					//()���̑�ꍀ�ڂ��ЂƂ�(,��؂�łȂ�)�Ȃ�萔
					//���}�[�N�̏���
					if(strchr(strArgArr[jj],0x40) != 0){
						strChg(strArgArr[jj],"@","");
						//(0���Ə����l�����ňȍ~�̏����ł��Ȃ�)
						ioTrgt_arg[ii].s_point = -99999;
						ioTrgt_arg[ii].value   = strArgArr[jj];
					}else{
						fprintf(stderr," Error C [%02d]:param parse error                       %s \n",11,local_server_time(strTime));
						exit(1);
					}
				}else{
					ioTrgt_arg[ii].s_point = atoi(strArgArr[jj]);
					//�����ɓ������̂ɊJ�n�ʒu��0�ȏ�łȂ����Ƃ͂��蓾�Ȃ�
					if(ioTrgt_arg[ii].s_point <= 0 || strlen(strArgArr[jj]) >= 10 || isStrDigit(strArgArr[jj])){
						fprintf(stderr," Error C [%02d]:Start point is incorrect          %s \n",15,local_server_time(strTime));
						exit(1);
					}
				}
				break;
			case 1:
				ioTrgt_arg[ii].length  = atoi(strArgArr[jj]);
				//�����ɓ������̂ɒ�����0�ȏ�łȂ����Ƃ͂��蓾�Ȃ�
				if(ioTrgt_arg[ii].length <= 0 || strlen(strArgArr[jj]) >= 10 || isStrDigit(strArgArr[jj])){
					fprintf(stderr," Error C [%02d]:Length is incorrect                 %s \n",14,local_server_time(strTime));
					exit(1);
				}
				break;
			case 2:
				ioTrgt_arg[ii].type    = strArgArr[jj];
				break;
			case 3:
				ioTrgt_arg[ii].operand = strArgArr[jj];
				break;
			case 4:
				if(strchr(strArgArr[jj],'@') == 0 || strchr((strchr(strArgArr[jj],'@') + 1),'@') == 0
					|| searchChr(strArgArr[jj],'@') > 2){
					fprintf(stderr," Error C [%02d]:@ Parentheses is not supported      %s \n",10,local_server_time(strTime));
					exit(1);
				}
				//���}�[�N�̏���
//				strChg(strArgArr[jj],"@","");
				*(strchr((strchr(strArgArr[jj],'@') + 1),'@')) = '\0';
				ioTrgt_arg[ii].value   = (strchr(strArgArr[jj],'@') + 1);
				break;
			default:
				break;
			}
		}
	}
	//�c���ڂ̏�����
	for(ii=ii; ii < ITEMLST; ii++){
		ioTrgt_arg[ii].s_point = 0;
		ioTrgt_arg[ii].value   = "";
	}

	return ret;
}

//�K�{���͓��̈����������������Ă��邩
//ioTrgt:�����I�u�W�F�N�g
//return �G���[1,����0
//author:koyama
int checkTrgtCorrect(Trgt_inf *ioTrgt){
	char strTime[] = "0000/00/00 00:00:00.000000";

	//���͂��Ȃ��Ƃ��͏o�͐�N���A
//	if(strlen(ioTrgt->ifi) == 0){
//		fprintf(stderr," Error [%02d]:Where is the input source  %s \n",01,local_server_time(strTime));
//		return 1;
//	}
	if(strlen(ioTrgt->ofi) == 0){
		fprintf(stderr," Error C [%02d]:Where is the output destination           %s \n",02,local_server_time(strTime));
		return 1;
	}
	return 0;
}

//*********************************************************
//���������֐�
//*********************************************************
int set_trgt(Trgt_inf *ioTrgt, char *iArgv){
	int ii,ret = 0, cnt =0;
	char wkstr[120+1] = "";
	char *strOptArr[20+1] = {'\0'};            //�I�v�V������
	char *strTmpArgv = iArgv;

	//�萔�̊Ԃ�SP����������u��
	chgChar(strTmpArgv,(char)0x20,(char)0x07,(char)0x40);
	//���������u �v�ŃI�v�V�����ʂɋ敪������
	int result = strSplit(strTmpArgv," ",strOptArr);

	for(ii=0; ii< result; ii++){
		chgChar(strOptArr[ii],(char)0x07,(char)0x20,(char)0x40);
		if (strstr(strOptArr[ii], "IFI") != NULL ) {
			//���̓t�@�C�����ݒ�
			ioTrgt->ifi = (strchr(strOptArr[ii],'=') + 1);
			continue;
		}else if (strstr(strOptArr[ii], "OFI=") != NULL ) {
			//�o�̓t�@�C�����ݒ�
			ioTrgt->ofi = (strchr(strOptArr[ii],'=') + 1);
			continue;
		}else if (strstr(strOptArr[ii], "MOD=") != NULL ) {
			//�������ݕ��@
			if((strstr(strOptArr[ii], "ADD") != NULL ) && (strlen(strOptArr[ii]) > 0)){
				//�ǉ��������݂̏ꍇ
				ioTrgt->mod = 0;
			}else if(((strstr(strOptArr[ii], "CRE") != NULL ) || (strstr(strOptArr[ii], "CREATE") != NULL )) && ( strlen(strOptArr[ii]) > 0)){
				//�V�K�������݂̏ꍇ
				ioTrgt->mod = 1;
			}else{
				//��L�ȊO
				ioTrgt->mod = -1;
			}
			continue;
		}else if (strstr(strOptArr[ii], "OUT=") != NULL ) {
			//�o�̓t�B�[���h�\���ݒ�
			ret = set_argv(ioTrgt->out, (strchr(strOptArr[ii],'=') + 1));            //�v�f����
			continue;
		}else if (strstr(strOptArr[ii], "SEL=") != NULL ) {
			//�Ώۏ����ݒ�
//			if((strstr(strOptArr[ii], ")A(") != NULL ) && (strlen(strOptArr[ii]) > 0)){
//				//Where��̏ꍇ(AND����)
//				ioTrgt->logical = 0;
//			}else if((strstr(strOptArr[ii], ")O(") != NULL ) && ( strlen(strOptArr[ii]) > 0)){
//				//Where��̏ꍇ(OR����)
//				ioTrgt->logical = 1;
//			}else{
//				//Where��ȊO�̏ꍇ
//				ioTrgt->logical = -1;
//			}
			ret = set_argv(ioTrgt->sel, (strchr(strOptArr[ii],'=') + 1));            //�v�f����
		}else if (strstr(strOptArr[ii], "PB3=") != NULL ) {
			//DOS�`���̃t�@�C���I�[�v�����[�h�ݒ�
			char strPara[20]="";
			if (strlen(strOptArr[ii]) > strlen("PB3=")){
				strcpy(strPara, (strchr(strOptArr[ii],'=') + 1));            //�w�肪����Ƃ��̂ݕϐ��֕���
			}
			if (strcmp(strPara, EXCLUSIVE) == 0) {
					//EXCLUSIVE
					strcpy(ioTrgt->opmod, "E");
			}else if (strcmp(strPara, PROTECT) == 0) {
					//PROTECT
					strcpy(ioTrgt->opmod, "P");
			}else if (strcmp(strPara, SHARE) == 0) {
					//SHARE
					strcpy(ioTrgt->opmod, "S");
			}else if (strPara[0] == '\0') {
					//(�w��Ȃ�)
					strcpy(ioTrgt->opmod, "S");            //�w�肪�Ȃ��Ƃ���SHARE�Ƃ���
			}else{
					//�w�肪�����ċK��ȊO
					strcpy(ioTrgt->opmod, "_");
			}
			if (strcmp(ioTrgt->opmod, "_") != 0){
				//�������[�h�ɂ��t�@�C���I�[�v�����[�h���m��
				if (ioTrgt->mod == 0){
					//AV-X->DOS
					strcat(ioTrgt->opmod, "I");
				}else if (ioTrgt->mod == 1) {
					//DOS->AV-X
					strcpy(ioTrgt->opmod, "SO");            //������̎w��ł�SHARE OUTPUT(SO)�ɒu������
				}
			}
			continue;
		}
	}

	return ret;
}


//*********************************************************
//int �� char �ϊ�
//*********************************************************
char *itoa( int val, char *str, int radix )
{
	char *p = str;
	unsigned int v = val;            // ��Ɨp(�ϊ��Ώۂ̒l)
	int n = 1;            // �ϊ�������̌����L���p
	while(v >= radix){					// ���������߂�
		v /= radix;
		n++;
	}
	p = str + n;            //�ŉ��ʂ̈ʒu����ݒ肷��
	v = val;
	*p = '\0';            // ������I�[�̐ݒ�
	do {
		--p;
		*p = v % radix + '0';            // 1���̐��l�𕶎��ɕϊ�
		if(*p > '9') {					// �ϊ�����������10�i�ŕ\���ł��Ȃ��ꍇ
			*p = v % radix - 10 + 'A';            // �A���t�@�x�b�g���g��
		}
		v /= radix;
	} while ( p != str);

	return str;
}


//
//�e�[�u����check
//
char * check_tablename(char *strTable){
	char *strpTarg;
	char *strpTemp;
	char strTemp[51]="`";

  if(strTable == NULL){
    //NULL�̏ꍇ�͂��̂܂ܕԂ�
    return strTable;
  }
	if(*(strTable + 0) != 0x60 && strlen(strTable) > 0){
		strpTarg  = strTable;
		//�ꕶ���ڂ͌��܂��Ă���̂�
		strpTemp = (strTemp + 1);
		while(*strpTarg != '\0'){
			if(*strpTarg != ' '){
				*strpTemp = *strpTarg;
				strpTemp++;
			}
			strpTarg++;
		}
		//�Ō��'`'��ǉ�
		*strpTemp = 0x60;
		*(strpTemp + 1) = '\0';
		strcpy(strTable,strTemp);
	}
	return strTable;
}


//�G���[�Ƀv���Z�X����t���ďo��
//in/out :fmt �o�̓t�H�[�}�b�g,...�t�H�[�}�b�g�ɑΉ�����ϐ�
//author : koyama
void mytool_runtime_error(char typeChar,const char *sfile,const char *fmt, ...){
	va_list ap;

	/* prefix */
	fprintf(stderr,"%s :",sfile);
	fflush (stderr);

	/* body */
	va_start (ap, fmt);
	vfprintf (stderr, fmt, ap);
	va_end (ap);

	/* postfix */
	fputs ("\n", stderr);
	fflush (stderr);

	//DBUNLOCK�����s
	if(typeChar == 'E'){
		//DBUNLOCK�����s
		setTableUnlock(NULL,NULL);
	}
}

//�ݒ�t�@�C���̃p�X���擾
//date:20150828
//auth: koyama
//�J�����gdir��conf�������J���Ƃǂ��łł����s�ł��Ȃ��̂�
char *getConfFilename(char *strConfPath){
	char strTime[] = "0000/00/00 00:00:00.000000";
	FILE *fpFileExist;      //20150828 add koyama

	if((fpFileExist = fopen(CONF_FLIEPATH, "r")) == NULL){
		if((fpFileExist = fopen(CONF_SHREPATH, "r")) == NULL){
			fprintf(stderr, "%s : Error C [%02d]:conf read Error %s \n",source_file_name, local_server_time(strTime));
			exit (1);
		}else{
			strcpy(strConfPath,CONF_SHREPATH);
		}
	}else{
		strcpy(strConfPath,CONF_FLIEPATH);
	}
	fclose(fpFileExist);

	return strConfPath;
}

//*********************************************************
//�ݒ�t�@�C���̓ǂݍ���
//*********************************************************
int conf_read(DB_SETTING *targSetting){
	int ii;
	//2015/08/28 kawda S ���[�J���� conf.xml ���Ȃ��Ƃ��̑Ή�
	char strConfPath[1024]; //20150828 add koyama

	myConfDebugFlg = 0;
	//�t�@�C���l�[�������Ƀ��[�_�|�C���^���쐬   //�t�@�C������ϐ��ɕύX 20150828
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//2015/08/28 kawda E
	//���[�_�����[�h�ł����Ԃ�
	xmlTextReaderRead(reader);
	//���݂̃m�[�h�̃|�C���^���Z�b�g�H
	xmlTextReaderExpand(reader);
	//���݂̃m�[�h����DOM�����o���Ă���?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc) return 1;
	//�h�L�������g����R���e�L�X�g()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
    if (!ctx) return 1;
	//xpath�Ŏw�肵���m�[�h���X�g���擾
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CONF_DEFPATH, ctx);
    if (!xpobj) return 1;
	//�m�[�h���X�g���m�[�h�̔z��̂悤�Ȃ��̂�
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//�m�[�h���̎擾(�擾�ł��Ȃ��Ȃ�0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//�m�[�h���X�g����l��\��
	for (ii = 0; ii < size; ++ii) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, ii);
			if (node->content) {
				//�ݒ�t�@�C������DB�̃z�X�g�����擾
				if(strcmp(node->parent->name,CONF_DB_HOST) == 0){
					strcpy(targSetting->db_server,node->content);
				}
				//�ݒ�t�@�C������DB�̃��[�U�����擾
				if(strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(targSetting->db_user,node->content);
				}
				//�ݒ�t�@�C������DB�̃p�X���[�h���擾
				if(strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(targSetting->db_password,node->content);
				}
				//�ݒ�t�@�C������DB�����擾
				if(strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(targSetting->db_database,node->content);
				}
				//�ݒ�t�@�C������|�[�g���擾
				if(strcmp(node->parent->name,CONF_DB_PORT) == 0){
					targSetting->db_port = atoi(node->content);
				}
				//�ݒ�t�@�C�����狤�L���[�h�p�̃e�[�u�������擾
				if(strcmp(node->parent->name,MANA_TBL_NAME) == 0){
					strcpy(targSetting->db_manatbl,node->content);
				}
				if(strcmp(node->parent->name,DEBUG_FLGNAME) == 0){
					//�Ԋ҂ł��Ȃ��������0�ɂȂ�͂�
					myConfDebugFlg = atoi(node->content);
				}
			} else {
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				return 1;
			}
		}
	}
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	return 0;
}

//�e�[�u����check
//add commment �e�[�u�����̑O��́u`�v���������Ă���
char *convertTablenameToVariable(char *strTable){
	char *strpTarg;
	char *strpTemp;
	char strTemp[51]="";

	if(strTable==NULL || strlen(strTable) == 0){
    //NULL�܂��͋󕶎���̂Ƃ��͏����Ȃ�
    return strTable;
	}
	if(*(strTable + 0) == 0x60){
		//2�����ڂ���
		strpTarg  = strTable + 1;
		strpTemp = (strTemp);
		while(*strpTarg != '\0'){
			if(*strpTarg != ' '){
				*strpTemp = *strpTarg;
				strpTemp++;
			}
			strpTarg++;
		}
		//�Ō��'`'������
		if(*(strpTemp - 1) == 0x60){
			*(strpTemp - 1) = '\0';
		}
		*(strpTemp) = '\0';
		strcpy(strTable,strTemp);
	}
	return strTable;
}
//--------�f�[�^�x�[�X�n--------�֐�Start
// -----------------------------------------------------Lock,Unlock Start
//##############################################################################
//#####  F I L E  O P E N  L O C K / U N L O C K ###############################
//##############################################################################
//##############################################################################
//##############################################################################
// ���j�[�NID����
//�Ԓl�F�������ꂽ�A��ӂ�ID
int getUid(){

	unsigned int now = (unsigned int)time( 0 );
	srand( now );
	// srand�֐��ŁA�����p�^�[��������������
	// �v���O�������s���ƂɈقȂ�p�^�[�����g����

	return rand();
}
//�����Ώۃt�@�C����LOCK����
//�����FtargSetting	DB�ݒ�\����
//�����Fitrgt		�󂯎������̏��\����
//����:lckTableName TableName��in,out�Ȃ̂ŊO���畡����ǂ�ł��炤
//�Ԓl�F���s����
int setTableLock(DB_SETTING *targSetting, Trgt_inf *itrgt,char *lckTableName){
	int intSub = 0, ret =0;
	int nn =0;
	char sqlStr[256]="";
	MYSQL_ROW	res;
	MYSQL_RES	*result;
	MYSQL_ROW	inres;
	MYSQL_RES	*inResult;
	char strUid[16]="";
	char strSS[8]="";
	char strTime[] = "0000/00/00 00:00:00.000000";            //���Ԃ̕�������i�[

  if(lckTableName==NULL || strlen(lckTableName)==0){
    //�e�[�u�����Ȃ��A�܂��́A�e�[�u���ϐ�NULL�̏ꍇ�͏����Ȃ�
    return 0;
  }
	//�Ǘ��e�[�u���X�V�p�g�����U�N�V����
	strcat(sqlStr, "START TRANSACTION; ");
	if(mysql_query(mysqlConn, sqlStr) != 0){
		mytool_runtime_error('E',source_file_name," Error C [%02d]:can't run the Transaction %s %s  %s ", 81, targSetting->db_manatbl, mysql_error(mysqlConn), local_server_time(strTime));
		DB_Close();
		return 1;
	}
	//�g�����ϐ������Z�b�g
	memset(sqlStr,'\0',strlen(sqlStr));

	// �Ώۃe�[�u���̃��b�N��Ԋm�F(���b�N����)
	//TableName��in,out�Ȃ̂�
	strcat(sqlStr,"SELECT LockMode FROM `");
	strcat(sqlStr,targSetting->db_manatbl);
	strcat(sqlStr,"` WHERE TableName = '");
	strcat(sqlStr,lckTableName);
	strcat(sqlStr,"';");

	if(mysql_query(mysqlConn, sqlStr) != 0){
		mysql_failure();
		ret = 1;
		return ret;
	}
	//�g�����ϐ������Z�b�g
	memset(sqlStr,'\0',strlen(sqlStr));

	if(result = mysql_store_result(mysqlConn)){
		int rr=0;

		rr=mysql_num_rows(result);

		// �����񏉊���
		memset(sqlStr,'\0',strlen(sqlStr));

		sprintf(strUid,"%d", unqId);
		//openMode�͌Œ�ɂ��Ă���
		//lckTableName��ofi�ƈ�v���Ă�����ofi,�����łȂ����ifi
    //ifi�͎w��Ȃ��̃P�[�X������̂�ofi�Ŕ��f
		if(strcmp(lckTableName,itrgt->ofi) == 0){
      sprintf(strSS,"%s", "SE");
		}else{
      sprintf(strSS,"%s", "SI");
		}

		//��s�̏������Ȃ���΂��̂܂ܐڑ�
		if(rr == 0){
			// �Ǘ��e�[�u���̓o�^
			strcat(sqlStr,"INSERT INTO `");
			strcat(sqlStr,targSetting->db_manatbl);
			strcat(sqlStr,"` ( TableName,Uid,LockMode,pg_name,user_name,LockDateTime) ");
			strcat(sqlStr,"VALUES ( '");
			strcat(sqlStr,lckTableName);
			strcat(sqlStr,"',");
			strcat(sqlStr,strUid);
			strcat(sqlStr,",'");
			strcat(sqlStr,strSS);
			strcat(sqlStr,"','");
			strcat(sqlStr,source_file_name);    //pg_name
			strcat(sqlStr,"','");
			strcat(sqlStr,source_user_name);    //pg_name
			strcat(sqlStr,"', now() );");
		}else{
			for(nn = 0;nn < rr;nn++){
				//
				char strDecision[5]="";
				res=mysql_fetch_row(result);

				//��s�̔���p2�����𔻒�ϐ��ɃR�s�[
				if (strcmp(res[0], "") != 0){
					memcpy(strDecision, res[0], 2);
				}else{
					memcpy(strDecision, "  ", 2);
				}
				memcpy((strDecision + 2), strSS, 2);
				//Lock�ł��邩(Lock�ł��Ȃ��ꍇ�͏����s��)�`�F�b�N
				if (
				    (*strDecision == 'E') ||
				    (
				     (*strDecision == 'P') &&
				     (
				      (strcmp(strDecision,"PIPI") != 0 && strcmp(strDecision,"PISI") != 0 &&
				       strcmp(strDecision,"P-SI") != 0 && strcmp(strDecision,"PESI") != 0)
				     )
				    ) ||
				    (
				     (*strDecision == 'S') &&
				     (
				      (strcmp(strDecision,"SIPI") != 0 && strcmp(strDecision,"SISI") != 0 &&
				       strcmp(strDecision,"SIP-") != 0 && strcmp(strDecision,"SIS-") != 0 &&
				       strcmp(strDecision,"SIPE") != 0 && strcmp(strDecision,"SISE") != 0 &&
				       strcmp(strDecision,"S-SI") != 0 && strcmp(strDecision,"S-S-") != 0 &&
				       strcmp(strDecision,"S-SE") != 0 && strcmp(strDecision,"SESI") != 0 &&
				       strcmp(strDecision,"SES-") != 0 && strcmp(strDecision,"SESE") != 0)
				     )
				    )
				   ){
					//��s�� E(XCLUSIVE)�Ȃ�Lock�ł��Ȃ�
					//P(ROTECT)�F(��s����s)��(PI��PI)(PI��SI)(P-��SI)(PE��SI)�ȊO��Lock�ł��Ȃ�
					//S(HARE)�F(��s����s)��(SI��PI)(SI��SI)(SI��P-)(SI��S-)(SI��PE)(SI��SE)(S-��SI)(S-��S-)(S-��SE)(SE��SI)(SE��S-)(SE��SE)�ȊO��Lock�ł��Ȃ�
						 mytool_runtime_error('E',source_file_name," Error C [%02d]:alredy locked %s:%s %s ", 82, lckTableName, targSetting->db_manatbl, local_server_time(strTime));
					ret = 1;
					break;
				}else{
				//�Ǘ��e�[�u����LockMode�̍X�V���K�v���`�F�b�N
					if (
					    (*strDecision == ' ') ||
					    (
					     (*strDecision == 'P') &&
					     (
					      (strcmp(strDecision,"PIPI") == 0)
					     )
					    ) ||
					    (
					     (*strDecision == 'S') &&
					     (
					      (strcmp(strDecision,"SIPI") == 0 || strcmp(strDecision,"SISI") == 0 ||
					       strcmp(strDecision,"SIP-") == 0 || strcmp(strDecision,"SIS-") == 0 ||
					       strcmp(strDecision,"SIPE") == 0 || strcmp(strDecision,"SISE") == 0 ||
					                                          strcmp(strDecision,"S-S-") == 0 ||
					       strcmp(strDecision,"SES-") == 0 || strcmp(strDecision,"SESE") == 0)
					     )
					    )
					   ){
					   	//(��)�F��(Lock�Ȃ�)��LockMode�u������
						//P(ROTECT)�F(��s����s)��(PI��PI)��LockMode�u������
						//S(HARE)�F(��s����s)��(SI��PI)(SI��SI)(SI��P-)(SI��S-)(SI��PE)(SI��SE)(S-��S-)(SE��S-)(SE��SE)��LockMode�u������
						// �Ǘ��e�[�u���̍X�V
						strcat(sqlStr,"UPDATE `");
						strcat(sqlStr,targSetting->db_manatbl);
						strcat(sqlStr,"` SET Uid = ");
						strcat(sqlStr,strUid);
						strcat(sqlStr,", LockMode = '");
						strcat(sqlStr,strSS);
						strcat(sqlStr,"', pg_name = '");
						strcat(sqlStr,source_file_name);
						strcat(sqlStr,"', user_name = '");
						strcat(sqlStr,source_user_name);
						strcat(sqlStr,"', LockDateTime = now() ");
						strcat(sqlStr," WHERE TableName = '");
						strcat(sqlStr,lckTableName);
						strcat(sqlStr,"' ");
						strcat(sqlStr,";");
					}
				}
			}
		}
		if(ret != 1 && *sqlStr != '\0'){
			// SQL�̎��s��COMMIT
			if(mysql_query(mysqlConn, sqlStr) != 0){
				mysql_failure();
				ret = 1;
				mytool_runtime_error('E',source_file_name," Error C [%02d]:can't lock %s ", 82, lckTableName);
				return ret;
			}else{
				//�g�����ϐ������Z�b�g
				memset(sqlStr,'\0',strlen(sqlStr));
				//�R�~�b�g
				strcat(sqlStr,"COMMIT; ");
				if(mysql_query(mysqlConn, sqlStr) != 0){
					mysql_failure();
					mytool_runtime_error('E',source_file_name," Error C [%02d]:can't lock %s ", 83, lckTableName);
					ret = 1;
					return ret;
				}
			}
		}
	}else{
		mytool_runtime_error('E',source_file_name," Error C [%02d]:%s", 84, mysql_error(mysqlConn));
		ret = 1;
	}
	mysql_free_result(result);

	return ret;
}
//�����Ώۃt�@�C����UNLOCK����
//�����FtargSetting	DB�ݒ�\����
//�����Fitrgt		�󂯎������̏��\����
//�Ԓl�F���s����
int setTableUnlock(DB_SETTING *targSetting, Trgt_inf *itrgt){
	int ret =0,intSub = 0,ii = 0;
	char sqlStr[256]="";
	char strUid[16]="";
	MYSQL_RES	*result;
	char strTime[] = "0000/00/00 00:00:00.000000";

	if(targSetting == NULL && itrgt == NULL){
		targSetting = background_targSetting;
		itrgt       = background_itrgt;
	}
	//BackGround�ɂ��Ȃ��Ƃ���Unlock����K�v�Ȃ�
	if(targSetting == NULL && itrgt == NULL){
		return 1;
	}

	//�Ǘ��e�[�u���X�V�p�g�����U�N�V����
	strcat(sqlStr, "START TRANSACTION; ");
	if(mysql_query(mysqlConn, sqlStr) != 0){
		DB_Close();
		// fprintf(stderr, " Error C [%02d]:can't run the Transaction %s %s          %s \n", 91, targSetting->db_manatbl, mysql_error(mysql), local_server_time(strTime));
		return 1;
	}
	//�g�����ϐ������Z�b�g
	memset(sqlStr,'\0',strlen(sqlStr));

	sprintf(strUid, "%d", unqId);

	strcat(sqlStr,"UPDATE `");
	strcat(sqlStr,targSetting->db_manatbl);
	strcat(sqlStr,"` SET Uid = NULL, LockMode = '");
	strcat(sqlStr, "    ");            //LOCKMODE DEFAULT VALUE
	strcat(sqlStr,"' , LockDateTime = now() ");
	strcat(sqlStr," WHERE Uid = ");
	strcat(sqlStr,strUid);
	strcat(sqlStr,";");

	// SQL�̎��s
	if(mysql_query(mysqlConn, sqlStr) != 0){
		// mysql_failure();
		ret = 1;
		return ret;
	}else{
		//�g�����ϐ������Z�b�g
		memset(sqlStr,'\0',strlen(sqlStr));
		//�R�~�b�g
		strcat(sqlStr,"COMMIT; ");
		if(mysql_query(mysqlConn, sqlStr) != 0){
			// mysql_failure();
			DB_Close();
			fprintf(stderr," Error C [%02d]:can't lock %s \n", 92, itrgt->ifi);
			ret = 1;
			return ret;
		}
	}

	return ret;
}
// -----------------------------------------------------Lock,Unlock END

//
//�ݒ�̓ǂݍ��݂ƃf�[�^�x�[�X�ւ̐ڑ����s��
//IN  �F targSetting->�f�[�^�x�[�X�ڑ����
//OUT �F �Ȃ�
//author:koyama
int db_init(DB_SETTING *targSetting){
	int result=0;
	char strTime[] = "0000/00/00 00:00:00.000000";

	if(conf_read(targSetting)){
		mytool_runtime_error('E',source_file_name," Error C [%02d] :can't read configuration file           %s ", 01, local_server_time(strTime));
		result = 1;
	}else{
		if(DB_Open(targSetting)){
			result = 1;
		}
	}
	//�G���[�ɂȂ������悤�ɃR�s�[���Ƃ��Ă���
	background_targSetting = targSetting;

	return result;
}

//�O���[�o���ϐ�����ݒ���擾���A�f�[�^�x�[�X�ɐڑ�����
//IN �F�Ȃ�
//OUT�F0����
//     1�ُ�
//author:koyama
int DB_Open(DB_SETTING *targSetting){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	char strAutocommit[] = "SET AUTOCOMMIT=0;";
	mysqlConn = mysql_init(NULL);

	if (!mysql_real_connect(mysqlConn, targSetting->db_server, targSetting->db_user, targSetting->db_password, targSetting->db_database, targSetting->db_port, NULL, 0)) {
		mytool_runtime_error('E',source_file_name," Error C [%02d]:Connect Error %s                    %s", 02,mysql_error(mysqlConn),local_server_time(strTime));
		//�ڑ��s��
//		raise(SIGABRT);
		ret = 1;
	}else{
	    //�ڑ���
	    ret = 0;
	}
	//�d�l�Ƃ���SJIS�����_���Ȃ̂�
	if(mysql_options(mysqlConn, MYSQL_SET_CHARSET_NAME, "SJIS") != 0){
		mytool_runtime_error('E',source_file_name, " Error C [%02d]:db init Error %s :%s\n",34, local_server_time(strTime),mysql_error(mysqlConn));
		return 1;
	}

	//AutoCommit Off
	ret = mysql_query(mysqlConn, strAutocommit);

	unqId = getUid();            //2015/08/28 kawada add �Ǘ��e�[�u���̍X�V�p�Ƀ��j�[�NID���擾����

    return ret;
}

//�ݒ�̒l���g���ăf�[�^�x�[�X�����
//IN �F�Ȃ�
//OUT�F0����
//     1�ُ�
//author:koyama
int DB_Close(){
	int ret =0;
	//DB�ڑ��̐ؒf

	// �Ǘ��e�[�u���p�̃R�l�N�V�����G���h
	setTableUnlock(NULL,NULL);
	//DB_init��ʂ��Ă��邱�Ƃ��m�F
	if(background_targSetting != NULL){
		//return void�Ȃ̂ŕK��0�ŕԂ�
		mysql_close(mysqlConn);
	}
	return ret;
}

//*********************************************************
//SQL���s�G���[���̌㏈��
// upd koyama function���ƍs�����o��悤�ɏC��
//*********************************************************
int __mysql_failure(const char *funcName,int funcLine){
	int ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	//�G���[���e���o��
	mytool_runtime_error(' ',source_file_name," Error C [%02d]: Query Error %20s %s:%d %s "
		,99,mysql_error(mysqlConn),funcName,funcLine,local_server_time(strTime));

	//���[���o�b�N
	ret = mysql_query(mysqlConn, "ROLLBACK");

	return ret;
}

//*********************************************************
//SQL���s
//*********************************************************
int mysql_run(char * iquery){
	int ret = 0;
	int query_size = 0;

	query_size = strlen(iquery);

//	printf("mysql_run :%s\n",iquery);            //�f�o�b�O�p
	ret = mysql_real_query(mysqlConn, iquery,query_size);

	if(ret != 0){
		mysql_failure();
		ret = 1;
	}

	return ret;
}

//�g�����U�N�V�����̃X�^�[�g�Ɗ֘A�̏������s��
//return ���s������1 ����I����0
//author:koyama 20170111
int executeStartTrunsaction(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  char strSql[1024]="";
  char strTime[] = "0000/00/00 00:00:00.000000";

  sprintf(strSql, "START TRANSACTION;");
	if(mysql_query(Connection,strSql) != 0){
    fprintf(stderr, " Error C [%02d]:trunsaction start Error %s %s\n", 31,local_server_time(strTime), mysql_error(mysqlConn));
		setTableUnlock(targSetting, itrgt);
    return 1;
  }
  return 0;
}

//�e���|�����[�e�[�u���̍쐬
//return ���s������1 ����I����0
//author:koyama 20170111
int executeCreateTemporary(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  char strSql[1024]="";

  sprintf(strSql, "CREATE TEMPORARY TABLE tmp (ID INT NOT NULL AUTO_INCREMENT PRIMARY KEY, ITEM VARBINARY(1024));");
  if(mysql_query(Connection, strSql) != 0){
    setTableUnlock(targSetting, itrgt);
    return 1;
  }
  return 0;
}


//�쐬�����e���|�����e�[�u�����^�[�Q�b�g�̏o�̓e�[�u����
//return ���s������1 ����I����0
//author:koyama 20170111
int executeTmpToTargTable(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  char strSql[2048]="";

  sprintf(strSql,"INSERT INTO %s (ITEM) SELECT ITEM FROM tmp ORDER BY ID;"
  ,check_tablename(itrgt->ofi));

	//infile���Ȃ��Ƃ��͒��g���폜���邾�� add 20141117 koyama
	if(itrgt->ifi != NULL && strlen(itrgt->ifi) > 0){
		//SQL���s
		if(mysql_query(Connection,strSql) != 0){
			setTableUnlock(targSetting, itrgt);
			//�G���[�̂Ƃ���mysql_failure��
			mysql_failure();
			DB_Close();
			return 1;
		}
	}
}

//�쐬����tmp�Ƃ������O�̃e���|�����e�[�u�����폜����
//return ���s������1 ����I����0
//author:koyama 20170111
int dropTemporaryTable(MYSQL *Connection,DB_SETTING *targSetting,Trgt_inf *itrgt){
  if(mysql_query(Connection,"DROP TABLE tmp;") != 0){
		setTableUnlock(targSetting, itrgt);
		//�G���[�̂Ƃ���mysql_failure��
		mysql_failure();
		DB_Close();
		return 1;
	}
  return 0;
}

//*********************************************************
//DB�ڍs����
//*********************************************************
int dbExecute(Trgt_inf *itrgt,DB_SETTING *targSetting){
	String_t query;            //�����ݒ��ǂݍ���SQL�̊i�[
	int  query_size = 0;            //SQL������̒�����ێ�����(�ő�l��sizeof(query)�܂�)
	char temp[256] = "";            //SQL�ptemporary
	void *buffer[100];
	char *strWhereArr[100] = {'\0'};            //where�傪�������鎞�̂��߂�
	char strType[64] = "";
	int ii,jj, ret = 0,item_cnt = 0;
	Argv_inf wkout[ITEMLST * 2];            //�\���p�t�B�[���h�z��
	char wkstr[100];
	char *wkstr_p;
	char wk_point[WK_INTSTR_LEN];
	char wk_length1[WK_INTSTR_LEN];
	char wk_length2[WK_INTSTR_LEN];
	char strTime[] = "0000/00/00 00:00:00.000000";
	char sqlstr[127] = "";            //SQ���X�e�[�g�����g(�Z�����̂͂�����ŏ���)


	//������
	for(ii = 0; ii < ITEMLST * 2 ; ii++){
		argv_ini(&wkout[ii]);
	}

	//�ݒ�̓ǂݍ���(�O���[�o���ϐ��֊i�[)
	if (conf_read(targSetting) != 0){
		fprintf(stderr, "conf read Error %s :%s\n", local_server_time(strTime),mysql_error(mysqlConn));
		return 1;
	}

	//DB�ڑ�
	if ((mysqlConn = mysql_init(mysqlConn)) == NULL) {
		fprintf(stderr, "db init Error %s :%s\n", local_server_time(strTime),mysql_error(mysqlConn));
		return 1;
	}
	if(mysql_real_connect(mysqlConn, targSetting->db_server, targSetting->db_user, targSetting->db_password, targSetting->db_database, targSetting->db_port, NULL, 0) == NULL) {
		fprintf(stderr, "db connect Error %s :%s\n", local_server_time(strTime), mysql_error(mysqlConn));
		return 1;
	}

	unqId = getUid();            //���j�[�NID���쐬 //add koyama �Ǘ��e�[�u���X�V�ɔ���
  if(checkTrgtCorrect(itrgt) == 1){
    //�G���[���b�Z�[�W�͊֐����ŏo��
    return 1;
  }
	//20160817 add koyama LockMode�X�V upd 20161109
	if ((setTableLock(targSetting, itrgt,convertTablenameToVariable(itrgt->ifi)) != 0 ) || (setTableLock(targSetting, itrgt,convertTablenameToVariable(itrgt->ofi)) != 0)){
		setTableUnlock(targSetting, itrgt);
		return 1;
	}

	//�g�����U�N�V�����X�^�[�g

	//////////////////     sql     /////////////////////
	// temporary�쐬
  //function��20170111
  if(executeCreateTemporary(mysqlConn,targSetting,itrgt)){
    string_fin(&query);
    return 1;
  }

	for(ii = 0; ii < ITEMLST ; ii++){
		if(itrgt->out[ii].s_point != 0) {
			//�\���Ώۂ̍���
			wkout[item_cnt].s_point = itrgt->out[ii].s_point;
			wkout[item_cnt].length  = itrgt->out[ii].length;
			wkout[item_cnt].type    = itrgt->out[ii].type;
			wkout[item_cnt].value   = itrgt->out[ii].value;

			item_cnt += 1;
		}else{
			break;
		}
	}

	//SQL������
	string_init(&query,   "INSERT INTO tmp (ITEM) (");
	string_concat(&query, " SELECT ");

	//item_cnt��0�̂Ƃ���item��ϊ����Ă���
	if(item_cnt != 0){
		string_concat(&query, " CAST(CONCAT(");

		for(ii = 0; ii < item_cnt ; ii++){
			if (ii != 0) string_concat(&query, " ,");
			string_concat(&query, " t1.item");
			itoa(ii+1,wkstr,10);
			string_concat(&query, wkstr);
		}
		string_concat(&query, " ) AS BINARY) AS item ");
	}else{
		string_concat(&query, " t1.item ");
	}
	string_concat(&query, " FROM (");
	string_concat(&query, " SELECT");
	string_concat(&query, " t2.rownum");

	//item_cnt��0�̂Ƃ���item��ϊ����Ă���
	if(item_cnt != 0){
		for(ii = 0; ii < item_cnt ; ii++){
			itoa(ii+1,wkstr,10);
			string_concat(&query, " ,t2.item");
			string_concat(&query, wkstr);
		}
	}else{
		string_concat(&query, " ,t2.item");
	}
	string_concat(&query, " ");
	string_concat(&query, " FROM (");

	string_concat(&query, " SELECT");
	string_concat(&query, " @ii:=@ii+1 AS rownum");

	//item_cnt��0�̂Ƃ���item��ϊ����Ă���
	if(item_cnt != 0){
		for(ii = 0; ii < item_cnt ; ii++){
			string_concat(&query, " ,");

			itoa(ii+1,wkstr,10);
			sprintf(wk_point,"%d",wkout[ii].s_point);
			sprintf(wk_length1,"%d",wkout[ii].length);
			sprintf(wk_length2,"%d",wkout[ii].length * 2);
			if((strcmp(wk_length1,"0") != 0) && (strlen(wkout[ii].value) == 0)){
				string_concat(&query, " MID(MDATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " ) AS item");
				string_concat(&query, wkstr);
			}else if(strlen(wkout[ii].value) != 0){
				string_concat(&query, " \"");
				string_concat(&query, wkout[ii].value);
				string_concat(&query, "\" AS item");
				string_concat(&query, wkstr);
			}
		}
	}else{
		string_concat(&query, " ,MDATA.item ");
	}
	string_concat(&query, " ");
	string_concat(&query, " FROM (SELECT @ii:=0) AS NUMBER,");
	string_concat(&query, check_tablename(itrgt->ifi));
	string_concat(&query, " AS MDATA ");

	//������
	for(ii = 0; ii < ITEMLST ; ii++){

		if((itrgt->sel[ii].s_point != 0) && (itrgt->sel[ii].length) != 0){
			char oper[OPER_LEN + 1] = "";
			sprintf(wk_point,"%d",itrgt->sel[ii].s_point);
			sprintf(wk_length1,"%d",itrgt->sel[ii].length);

			if (ii == 0){
				string_concat(&query, " WHERE ");
			}else if(itrgt->sel[ii].logical == 0){
				string_concat(&query, " AND ");
			}else if(itrgt->sel[ii].logical == 1){
				string_concat(&query, " OR ");
			}

			//��r���Z�q
			if(strcmp(itrgt->sel[ii].operand,"EQ") == 0){
				memcpy(oper, " =  ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"GE") == 0){
				memcpy(oper, " >= ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"GT") == 0){
				memcpy(oper, " >  ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"LE") == 0){
				memcpy(oper, " <= ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"LT") == 0){
				memcpy(oper, " <  ",OPER_LEN);
			}else if(strcmp(itrgt->sel[ii].operand,"NE") == 0){
				memcpy(oper, " != ",OPER_LEN);
			}


			if(itrgt->sel[ii].type[0] == 'S'){
				//�v���X
				//CAST(MID(MDATA.item, 15 , (9 - 1) ) as SIGNED) * 10
				//+ CAST(SUBSTR(HEX(MID(MDATA.item, 15 + (9 - 1), 1 )),1,2 ) as SIGNED)
				string_concat(&query, " ((MID(HEX(MID(MDATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1)),1,1) ");
				string_concat(&query, " = '3' AND ((CAST(MID(MDATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(MDATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1 )),2,1 ) as SIGNED) ) ");
				//��r���Z�q
				string_concat(&query, oper);
				//�l�̔�r
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) )");
				string_concat(&query, " OR ");
				//�}�C�i�X
				//CAST(MID(MDATA.item, 15 , (9 - 1) ) as SIGNED) * 10
				//+ CAST(SUBSTR(HEX(MID(MDATA.item, 15 + (9 - 1), 1 )),1,2 ) as SIGNED) * (-1)
				string_concat(&query, " (MID(HEX(MID(MDATA.item,");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1),1)),1,1) ");
				string_concat(&query, " = '7' AND (((CAST(MID(MDATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " , (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1) ) as SIGNED) * 10 ");
				string_concat(&query, " + CAST(SUBSTR(HEX(MID(MDATA.item, ");
				string_concat(&query, wk_point);
				string_concat(&query, " + (");
				string_concat(&query, wk_length1);
				string_concat(&query, " - 1), 1 )),2,1 ) as SIGNED)) * (-1)) ");
				//��r���Z�q
				string_concat(&query, oper);
				//�l�̔�r
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) ) )");
			}else if(itrgt->sel[ii].type[0] == 'P' ||itrgt->sel[ii].type[0] == 'Q'){
				//�����Ȃ�
				//SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,(CEIL((4 + 1) / 2) * 2)) = 'C'
				//AND
				//(CAST(SUBSTR(HEX(MID(ITEM,39,CEIL((4 + 1) / 2))) ,1,(CEIL((4 + 1) / 2) * 2)) as SIGNED) <= 0)
				string_concat(&query, "( (SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2)) = 'F' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " , ");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) as SIGNED) ");
				//��r���Z�q
				string_concat(&query, oper);
				//�l�̔�r
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) )");
				string_concat(&query, " OR ");
				//�v���X
				string_concat(&query, " (SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2)) = 'C' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " , ");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,(");
				string_concat(&query, wk_length1);
				string_concat(&query, "  * 2)) as SIGNED) ");
				//��r���Z�q
				string_concat(&query, oper);
				//�l�̔�r
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) )");
				string_concat(&query, " OR ");
				//�}�C�i�X
				string_concat(&query, " (SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) = 'D' ");
				string_concat(&query, " AND (CAST(SUBSTR(HEX(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " , ");
				string_concat(&query, wk_length1);
				string_concat(&query, " )) ,1,(");
				string_concat(&query, wk_length1);
				string_concat(&query, " * 2)) as SIGNED) * -1 ");
				//��r���Z�q
				string_concat(&query, oper);
				//�l�̔�r
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ) ) )");
			}else if(itrgt->sel[ii].type[0] == 'N'){
				//���l��r�̏ꍇ
				string_concat(&query, " CAST(MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " ) as UNSIGNED)");
				//��r���Z�q
				string_concat(&query, oper);
				//�l�̔�r
				string_concat(&query, " CAST(\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\" as SIGNED) ");

			}else{
				//������r�̏ꍇ
				string_concat(&query, " MID(MDATA.ITEM,");
				string_concat(&query, wk_point);
				string_concat(&query, " ,");
				string_concat(&query, wk_length1);
				string_concat(&query, " )");
				//��r���Z�q
				string_concat(&query, oper);
				//�l�̔�r
				string_concat(&query, "\"");
				string_concat(&query, itrgt->sel[ii].value);
				string_concat(&query, "\"");
			}

		}else{
			break;
		}
	}

	string_concat(&query, " ) AS t2 ");
	string_concat(&query, " ORDER BY t2.rownum ");
	string_concat(&query, " ) AS t1 ");
	string_concat(&query, " );");

	////////////////////////////////////////////////////

	//infile���Ȃ��Ƃ��͒��g���폜���邾�� add 20141117 koyama
//	fprintf(stderr," : %s : \n",query);
	if(itrgt->ifi !=NULL && strlen(itrgt->ifi) > 0){
		//SQL���s
		if(mysql_query(mysqlConn,query.data) != 0){
			mysql_failure();
			setTableUnlock(targSetting, itrgt);
			//�G���[�̂Ƃ���mysql_failure��
			DB_Close();
			string_fin(&query);
			return 1;
		}
	}

	if(itrgt->mod == 1){
    //�ڍs��e�[�u���̃N���A
		string_init(&query,   "TRUNCATE TABLE ");
		string_concat(&query, check_tablename(itrgt->ofi));
		string_concat(&query, ";");
		//SQL���s
		if(mysql_query(mysqlConn,query.data) != 0){
			setTableUnlock(targSetting, itrgt);
			//�G���[�̂Ƃ���mysql_failure��
			mysql_failure();
			DB_Close();
			string_fin(&query);
			return 1;
		}
	}

	//tmp�˃\�[�g�Ώۂ�
  //infile���Ȃ��Ƃ��͒��g���폜���邾�� add 20141117 koyama
  //function�� 20170111 koyama
  if(executeTmpToTargTable(mysqlConn,targSetting,itrgt)){
    string_fin(&query);
    return 1;
  }

	//�e���|�����e�[�u���̍폜
  //function�� 20170111 koyama
  if(dropTemporaryTable(mysqlConn,targSetting,itrgt) == 1){
    string_fin(&query);
    return 1;
  }

	//�R�~�b�g���s
	//�R�~�b�g upd koyama �����܂ŗ��Ă�����OK�Ȃ̂ŃR�~�b�g
	ret = mysql_query(mysqlConn,"COMMIT");

	setTableUnlock(targSetting, itrgt);
	string_fin(&query);

	//DB�ڑ��̐ؒf
	mysql_close(mysqlConn);

	return ret;
}


//author:koyama
int main(int argc, char *argv[]){
	int ii,ret =0;
	char strTime[] = "0000/00/00 00:00:00.000000";
	DB_SETTING targSetting;
	Trgt_inf *trgt;
	trgt = (Trgt_inf *)malloc(sizeof(Trgt_inf)); //�������m��

	// �ϐ�������
	memset(source_file_name, '\0', PATHNAME_SIZE);
	//�G���[�n���h�����Z�b�g
	setErrorHundle();
	//DB�̐ڑ�
	if(db_init(&targSetting)){
		fprintf(stderr," Error [%02d] %s : DB Initialize Error \n", 00,local_server_time(strTime));
		exit(EXIT_FAILURE);
	}
	//������
	trgt_ini(trgt);

	//�������S���Ȃ��̓_��(�v���O�������͕K������)
	if(argc <= 1){
		fprintf(stderr," Error C [%02d]: Option is nothing                        %s \n", 00,local_server_time(strTime));
		exit(EXIT_FAILURE);
	}

	//�v���Z�X���ƃ��[�U�����擾
	getUserAndProcessName(source_file_name, source_user_name);

	myoptions = malloc(sizeof(char) * (strlen(argv[argc-1]) + 1));
	memset(myoptions,'\0',(strlen(argv[argc-1]) + 1));
	strcpy(myoptions,argv[argc-1]);
//	myoptions = argv[argc-1];
//	fprintf(stderr," Error C [%02d]: %s %s \n", 00,local_server_time(strTime),myoptions);

	for(ii=1;ii < argc; ii++){
		//���������u_�v�ŃI�v�V�����ʂɋ敪������
		ret = set_trgt(trgt,argv[ii]);
	}

	// DB����
	ret = dbExecute(trgt,&targSetting);

	// ���������
	free(trgt);
	free(myoptions);

	return ret;
}
