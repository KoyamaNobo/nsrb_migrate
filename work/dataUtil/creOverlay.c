#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <setjmp.h>
#include <math.h> 
#include <time.h>                /* localtime�ɕK�v */
#include <mysql.h>
#include <libxml/xmlreader.h>    /*conf�t�@�C����xml��ǂނ���*/
#include <libxml/xpath.h>        /*conf�t�@�C����xml��ǂނ���*/
#include <sys/types.h>           //�t�@�C���̏����擾���邽��
#include <sys/stat.h>            //�t�@�C���̏����擾���邽��
#include <iconv.h>               //�v�����^�֕����𑗂鎞�̕����R�[�h�ϊ�
#include <libcob.h>
#include "hpdf.h"
#include <confheader.h>
#include <ctype.h>
#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN];
int MT_Initialize();
#endif
//////MIN - �ŏ��l�擾 -
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#ifndef CONF_PATH
#define CONF_PATH
const char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";
const char* CONF_DEFPATH  = "//conf/*/text()";    //�g�b�v���x������꒼����\��
#endif

//DB�̃^�O��
#ifndef CONF_DB_TAG
#define CONF_DB_TAG
const char* CONF_DB_HOST = "dbHost";
const char* CONF_DB_USER = "dbUser";
const char* CONF_DB_PASS = "dbPass";
const char* CONF_DB_NAME = "dbName";
const char* CONF_DB_PORT = "dbPort";
const char* ITEM_TBL_NAME = "dbIEName";
const char* MANA_TBL_NAME = "dbTMName";
#endif


//DB�̐ݒ�(���������̔{�܂Ŏ���悤�ɂ��Ă���(mysql�̎d�l����Ƃ���))
const char* PR_DEFAULT_ENCODE = "90ms-RKSJ-H";
const char* PR_TEMP_PATH = "prnTempPath";
const char* PR_BASE_PATH = "prnBasePath";
const char* PR_PAGE_MAX  = "prnPageMax";
const char* PR_OBJ_MAX   = "prnObjMax";
const char* PR_TMP_FNAME = "FRTXXXXXX";
const char* PR_PRN_FNAME = "PRTXXXXXX";
char* CHAR_SET_DEFPATH  = "//conf/fontSet/fontElem";
#define CMARGIN 2
#define FILE_LENGTH 128
#define MARGIN_CONDITION 0.5

char PR_db_server[64];
char PR_db_user[34];
char PR_db_password[64];
char PR_db_database[64];
int  PR_db_port;

//�f�o�O�p�}�N�����i�}�N��������=�f�o�O���[�h�A�}�N�����Ȃ�=�ʏ탂�[�h�j
//#define DEBUG
//�t�H�[�}�b�g(�p���T�C�Y)����p�}�N�����i�}�N��������=SD��`���p���T�C�Y����A�}�N�����Ȃ�=M_FORM�Asize�w��ɏ]���j
//#define FORMAT

HPDF_Doc  pdf;
//HPDF_Page PR_page[2048];
HPDF_Page PR_page;
HPDF_PageSizes PR_pagesize;         //�y�[�W�̃T�C�Y
HPDF_PageDirection PR_pagestyle;    //�y�[�W�̏c��

//pdf�̗p���ݒ�(�t�H���g�̐ݒ���܂�)
char PR_Lineprint = 0;
float PR_fontsize = 0;                //pdf�̃t�H���g�T�C�Y
float PR_linepitch = 0,PR_charpitch = 0;         //�y�[�W�ɑ΂��镶���z�u��ݒ�
float PR_topmargin = 0,PR_leftmargin = 0;        //�]���̊(�㉺,���E�͓������Ƃ���)
char PR_fontname[31];                       //pdf�̃t�H���g�����i�[
const char* PR_font;                       //pdf�̃t�H���g���i�[
//pdf�̃I�u�W�F�N�g�̃v���p�e�B
int PR_currentLine = 0;                     //���ɏ������݂���s�ԍ�
//int PR_currentPage = 0;                     //���ݏ������ݒ��̃y�[�W�ԍ�
int PR_currentObj = 0;                     //���ݏ������ݒ��̃t�@�C���ԍ�
int PR_maxPage     = 0;                         //�t�@�C���ԍ��̍ő�
int PR_maxObj      = 0;                         //�t�@�C���ԍ��̍ő�
char *PR_fileArray[100] = {0};        //�t�@�C���|�C���^�̔z��ő�y�[�W����PR_maxObj*100�ɂȂ�

char PR_id[21];						//pdf�ݒ��ID
char PR_overlayfile[128] = "";		//�����I�[�o���C�t�@�C��(�p�X+�t�@�C��)
//char PR_overlayfileid[128] = "";	//�����I�[�o���C�t�@�C��ID(�g���q�Ȃ�)
char PR_overlaysearch[128] = "";	//�����I�[�o���CID�����p������(�e�[�u���FM_FORM���Q�Ƃ���)
char PR_overlaypdf[128] = "";		//�����I�[�o���Cpdf�t�@�C��ID

char PR_temppath[128] = "";        //�쐬�����pdf�̖��O
char PR_tempname[128] = "";        //�쐬�����pdf�̖��O
char PR_bindname[128] = "";         //�������pdf�̖��O
char PR_basepath[128] = "";                  //�w�i�ƂȂ�pdf�܂ł̃p�X
char PR_basename[128] = "";                  //�w�i�ƂȂ�pdf�̖��O
char PR_printername[31] = "";
jmp_buf PR_env;    //�G���[�����p

int gfuncid = 0;	//�֐�ID
int gtermid = 0;	//����ID

char gcutbuf[351];	//������؂�o��(getStr)�Ŏg�p����ϐ�
//char gstrup[2048];	//�啶����������(toLower)�Ŏg�p����ϐ�

//�󎚃T�C�Y����R�[�h�p�ϐ�
HPDF_REAL PR_lineheight = 0;		//�s��
HPDF_REAL PR_horizontalScale = 0;	//��������
HPDF_Font PR_definition_font;		//�t�H���g
int PR_ctrlCodeNoSave = 0;			//����R�[�h�ԍ�(�O�񔻒�p�ۑ�)(�����s�b�`�p)�i1:�S�p����=1.5�o�C�g�A2:�S�p����=2�o�C�g�j
int PR_ctrl2CodeNoSave = 0;			//����R�[�h�ԍ�(�O�񔻒�p�ۑ�)(���{�A�c�{�A�Q�{�w��p)�i3:��2�{�����A4:�c2�{�����A5:2�{�����A6:���������Z�b�g�A7:2�{�������Z�b�g�A8:�c�������Z�b�g�j
float PR_charsize = 0;				//���p�����̉����|�C���g�l

#define OVERLAY_REC_LEN 72	//�����I�[�o���C�t�@�C���̍ő僌�R�[�h��
#define FDD_LEN 35			//��`FD�g�p�̈撷
#define SDD_LEN 29			//��`FD�g�p�̈撷
#define ITD_LEN 56			//��`FD�g�p�̈撷
#define CAPTION_LEN 8		//�����o���l�̒�(���F49�`56)
#define CONTINUE_LEN 46		//�����o���l�̌p���ő咷(���F11�`56)
float ranhabaUnit = 0.72;	//�����P��(����=1(=1/100�C���`)�̃|�C���g���F1�C���`=72�|�C���g)

float A3_Height = 16.54;	//A3��
float A3_Width = 11.69;		//A3��
float B4_Height = 14.33;	//B4��
float B4_Width = 10.12;		//B4��
float A4_Height = 11.69;	//A4��
float A4_Width = 8.27;		//A4��

float divCharSpace = 10;	//CharSpace�����l
float rateBold = 1.25;		//������
float rateMed = 1.125;		//������
float rateThin = 1.0;		//�א���

//�����I�[�o���C�t�@�C���̓��e(�\���̒�`)
//(��`FD�R�[�f�B���O�p����51���܂Ł^���g�p��35��)
//(��`SD�R�[�f�B���O�p����51���܂Ł^���g�p��29��)
//(��`IT�R�[�f�B���O�p����72���܂Ł^���g�p��56���A���o���l�̌p���l��A���ۑ�����̂�72+350���m�ۂ��Ă���)
//(�����I�[�o���C��`�t�@�C���̍s����40�s���炢�Ȃ̂�100�s�p�ӂ��Ă���)
typedef struct overlay_data{
	char fdd[51];			//FD��`
	char sdd[51];			//SD��`
	char itd[423][100];		//IT��`
	int itd_cnt;			//IT��`����
}OVERLAY_DATA;
#define ITD_LEN_MAX 422	//IT��`�̍ő咷
#define GOVERLAY_DATA_MAX 100	//�����I�[�o���C�t�@�C���̒�`IT�̍ő匏��

//�����I�[�o���C�t�@�C���̓��e(�\���̒�`)
//(���o���l��54��(�ʏ�=8��+�p��=46))
//(�����I�[�o���Cpdf�t�@�C���̓��e�͕ҏW�ɂ���ď����I�[�o���C��`�t�@�C���̍s����葝����̂�200�s�p�ӂ��Ă���)
typedef struct pdf_data{
	int no;					//�A��
	int levelNo;			//���x��(�ԍ�)
	int beginRow;			//�J�n�ʒu�E�s
	int beginCol;			//�J�n�ʒu�E��
	int sizeRow;			//�傫���E�s
	int sizeCol;			//�傫���E��
	int lineStyl;			//���`�@(1:�c��(=V)�A2:����(=H)�A3:�����`(=B)�A5:�ΐ�(=O))
	int lineVar;			//����@(1:�����A2:�א��A3:���_���A4:�ד_���A9:����_�����A10:�׈�_����(=A)�A11:����(=O)�A12:���_��(=P)�A13:����_����(=S))
	int linePos;			//�ʒu�@(0:����(=D)�A1:����(=C))
	int endRow;				//�I���ʒu�E�s
	int endCol;				//�I���ʒu�E��
	float point;			//�|�C���g�@(0.8:80%����(8=9.0P)�A0.9:90%����(9=9.6P)�A1.5:150%����(Y=14.4P)�A2.0:200%����(Z=216P)�A1.0:100%����(�f�t�H���g�A���̑��w��̂Ƃ�))
	int zoom;				//�g��@(0:�Ȃ��A1:����(=T)�A2:����(=F))
	int ranhaba;			//�����@(�������[���玟�̕����̍��[�܂ł̑傫�����w��A�P��:1/100�C���`)
	int interval;			//����s�P��Ԃ̃o�C�g��(�擪�̒P��̓[����ݒ�)
	float spaceCharLen;		//�����ԋ󔒁i�P��=������(������)�F���o���l�A�����`�t���p�j
	char text[351];			//���o���l
}PDF_DATA;
int text_len = 350;			//���o���l�̍ő啶����

#define GPDF_DATA_MAX 500	//�����I�[�o���Cpdf�t�@�C���̓��e�̍ő匏��
PDF_DATA gpdf_data[GPDF_DATA_MAX];	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)
int gpdf_data_cnt = 0;		//�����I�[�o���Cpdf�t�@�C���̓��e�̌���

//IT��`�E�r���̃��x�����e(�\���̒�`)
//(�z��v�f�̓Y���� + 1 �����x���ԍ��ɑΉ�����)
typedef struct level_data{
	int levelNo;			//���x��(�ԍ�)
	int lineStyl;			//���`�@(1:�c��(=V)�A2:����(=H)�A3:�����`(=B)�A5:�ΐ�(=O))
	int beginRow;			//�J�n�ʒu�E�s
	int beginCol;			//�J�n�ʒu�E��
	int sizeRow;			//�傫���E�s
	int sizeCol;			//�傫���E��
}LEVEL_DATA;
int level_data_len = 10;	//IT��`�E�r���̃��x�����e�̍ő匏��

LEVEL_DATA glevel_data[9];	//IT��`�E�r���̃��x�����e(�\���̎���)

//���p���S�p�@�ϊ��p
#define ISKANJI(c)       (c >= 0x81 && c <= 0x9f || c >=0xe0 && c <= 0xfc)	//�����R�[�h�͈�
#define ISALPH(c)        (c >= 0x20 && c <= 0x7e)	//���p�p�����R�[�h�͈�
#define ISKANA(c)        (c >= 0xa1 && c <= 0xdf)	//���p�J�i�R�[�h�͈�
#define ISDAKU(c)        ((c & 0xff) == 0xde)	//���p�J�i���_�R�[�h
#define ISHANDAKU(c)     ((c & 0xff) == 0xdf)	//���p�J�i�����_�R�[�h
#define MAYBEDAKU(c)     (c >= 0xb6 && c <= 0xc4 || c >= 0xca && c <= 0xce)	//���_���t���\���̂��锼�p�J�i�R�[�h�͈�
#define MAYBEHANDAKU(c)  (c >= 0xca && c <= 0xce)	//�����_���t���\���̂��锼�p�J�i�R�[�h�͈�
unsigned char *ALPH[] = {"�@", "�I", "�h", "��", "��", "��", "��", "�f", "�i", "�j", "��", "�{", "�C", "�|", "�D", "�^", 
						"�O", "�P", "�Q", "�R", "�S", "�T", "�U", "�V", "�W", "�X", 
						"�F", "�G", "��", "��", "��", "�H", "��", 
						"�`", "�a", "�b", "�c", "�d", "�e", "�f", "�g", "�h", "�i", "�j", "�k", "�l", "�m", "�n", "�o", "�p", "�q", "�r", "�s", "�t", "�u", "�v", "�w", "�x", "�y", 
						"�m", "��", "�n", "�O", "�Q", "�e", 
						"��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", "��", 
						"�o", "�b", "�p", "�`"};
//�\=�z���31�Ԗ�
unsigned char *KANA[] = {"�B", "�u", "�v", "�A", "�E", 
						"��", "�@", "�B", "�D", "�F", "�H", "��", "��", "��", "�b", "�[", 
						"�A", "�C", "�E", "�G", "�I", 
						"�J", "�L", "�N", "�P", "�R", 
						"�T", "�V", "�X", "�Z", "�@", 
						"�^", "�`", "�c", "�e", "�g", 
						"�i", "�j", "�k", "�l", "�m", 
						"�n", "�q", "�t", "�w", "�z", 
						"�}", "�~", "��", "��", "��", 
						"��", "��", "��", "��", "��", 
						"��", "��", "��", "��", "��", 
						"�J", "�K"};
unsigned char *DAKU[] = {"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�K", "�M", "�O", "�Q", "�S", 
						"�U", "�W", "�Y", "�[", "�]", 
						"�_", "�a", "�d", "�f", "�h", 
						"�@", "�@", "�@", "�@", "�@", 
						"�o", "�r", "�u", "�x", "�|", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@"};
unsigned char *HDAK[] = {"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�p", "�s", "�v", "�y", "�|", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@", "�@", "�@", "�@", 
						"�@", "�@"};

HPDF_Point mcurpos;

//�G���[����
#ifdef HPDF_DLL
void  __stdcall
#else
void
#endif
error_handler(HPDF_STATUS error_no,HPDF_STATUS detail_no,void *user_data){
    fprintf(
    	stderr,
    	"  Error C [%02d-%02d]: error_no=%04X, detail_no=%u : %s\n", 
    	88, 88,
    	(HPDF_UINT)error_no,
    	(HPDF_UINT)detail_no,
    	map_source_func
    );
    longjmp(PR_env, 1);
}

static int (*func)(char *errno, const char *errmsg);
void mybacktrace(void);    //debug�p�֐�

//==============================================================================
//�y�ݒ���擾���āA�����I�[�o���C�t�@�C���������ǂݍ��ݏ����I�[�o���Cpdf�t�@�C���쐬�p�̍\���̂ɒl��ҏW�z�i01�j
int PR_Initialize();
//�ypdf���쐬�z�i02�j
int PR_Open();
//�y�����I�[�o���Cpdf�t�@�C��(�\����)���Q�Ƃ��ăI�[�o���Cpdf�t�@�C�����o�͂���z�i03�j
int PR_Write();
//�ypdf���o�͂���z�i04�j
int PR_Close();
//�y�y�[�W�̐����z�i05�j
int PR_NewPage();
//�ypdf���������݁A�ۑ��z�i06�j
int PR_Save();
//�yxml�ݒ�t�@�C���̓ǂݍ��݁z�i07�j
int PR_conf_read();
//�ydb���[�f�[�^��ǂݍ���Őݒ�l��ݒ�z�i08�j
int PR_setProperty();
//�y�����I�[�o���Cpdf�t�@�C���̏����쐬�z�i09�j
int PR_putInitoverlaypdf();
//�y�����I�[�o���C�t�@�C���̓ǂݍ��݂ƍ\���̂ւ̊i�[�z�i10�j
int PR_getoverlaydata(OVERLAY_DATA *loverlay_data);
//�y�����I�[�o���C��`�t�@�C������쐬���������I�[�o���C�t�@�C���̓��e�\���̂������Q�Ƃ���
//�@�����I�[�o���Cpdf�t�@�C���̓��e�\���̂Ƀf�[�^���쐬����z�i11�j
int PR_edtpdfData(OVERLAY_DATA *loverlay_data);
//�y�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�Ɍ��o���l�f�[�^���쐬�z�i12�j
int PR_edtpdfDataText(OVERLAY_DATA *loverlay_data, int pidx, int boxF, int *befRow, int *cnt);
//�y�����I�[�o���Cpdf�t�@�C���̓��e�\���̂Ɍr���f�[�^���쐬�z�i13�j
int PR_edtpdfDataLine(OVERLAY_DATA *loverlay_data, int pidx, int *cnt);
//�y�����I�[�o���C�t�@�C���̓��e(�\����)�̌��o���l�v�f���J�n�ʒu�E�s�A���ŕ��בւ��z�i14�j
void PR_sortoverlaydata(OVERLAY_DATA *loverlay_data);
//�y�����I�[�o���Cpdf�t�@�C���̓��e�\���̂Ɍr���f�[�^���쐬(���x��=01�A�����`�̌J��Ԃ��p)�z�i15�j
int PR_edtpdfDataLineLoop(OVERLAY_DATA *loverlay_data, int pidx, int *cnt);
//�y����������(�����I�[�o���CID)����t�@�C�����AID�����p������Apdf����ҏW�z�i16�j
int PR_edtFileID(char *parastr);

//�y�t�H���g���A�p���w��z�i51�j
int fontSetting();
//�y�ݒ�t�@�C���̃p�X���擾�z�i52�j
char *getConfFilename(char *strConfPath);
//�y�t�H���g�t�@�C���̃p�X�{�t�@�C�������擾�z�i53�j
char *getFontFilePath(char *fontname,char *filePath);
//�y�t�@�C���̗L�����m�F�z�i54�j
int chkFileExist(char *chkFile);
//�y���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ��z�i56�j
char *local_server_time(char *retStr);
//�y���p���S�p�ϊ��z�i57�j
void cnvHanToZen(const char *han, char *zen);
//�y�����_(�|�C���g)�A���ʒu�A������(�|�C���g)���猅�ʒu�̃|�C���g�l�����߂�z�i58�j
HPDF_REAL getColpos(HPDF_Point basPos, int Col, HPDF_REAL charWidth);
//�y�����_(�|�C���g)�A�s�ʒu�A������(�|�C���g)����s�ʒu�̃|�C���g�l�����߂�z�i59�j
HPDF_REAL getRowpos(HPDF_Point basPos, int Row, HPDF_REAL lineHeight, int drawline, int centerline);
//�y������̎w��ʒu����w�蒷���������o���z�i60�j
char *getStr(char *sbuf, int pos, int getlen);
//�y������̎w��ʒu����w�蒷���������o���Đ��l��(int�^�̂�)�z�i61�j
int getNum(char *sbuf, int pos, int len);
//�y������̕������𐔂���(�S�p�������P����)�z�i62�j
int getCharCnt(char *sbuf);
//�y�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�������Z�z�i63�j
int addpdf_dataCnt(int *cnt);
//�y�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���������z�i64�j
void clrpdf_data(int idx);
//�yIT��`�E�r���̃��x�����e(�\���̎���)���������z�i65�j
void clrlevel_data();
//�y���o���l�̕ҏW�i�c�����E�������j�z�i66�j
int edtpdfText(OVERLAY_DATA *loverlay_data, int pidx, int *cnt, char *buf, int *retRow);
//�y�|�C���g�E�g��ɂ��f�t�H���g����������������ꍇ�ɂP�s���Z�z�i67�j
void sortoverlaydataRowSet(char *s_itd, int *Row);
//�y�����I�[�o���Cpdf�t�@�C���̓��e�\���̂̍s�A���𒲐�����z�i68�j
int setpdfDataColRow(int *cnt);
//�y���x���w��z����IT�w�肪�\�`�����`�F�b�N�z�i69�j
int chkHyo(int lineStyl, int sizeRow, int sizeCol, int yrep, int xrep);
//�y������E���̋󔒕������폜�z�i70�j
void RTrim(char *str);
//�y������̑啶�����������ɕϊ��z�i71�j
char *strToLower(char *str);
//==============================================================================

//�ycreOverlay������z
int main(int argc, char *argv[]){
	int ret = 0;
	char strTime[] = "00/00/00 00:00:00";	//���Ԃ̕�������i�[

	//�����̐����m�F(�����I�[�o���CID)
	if (argc < 2){
		cob_runtime_error( " Error C [%02d-%02d] %s : Option is nothing ", 00, 01, local_server_time(strTime));
		exit(EXIT_FAILURE);
	}

	//�����̒������m�F(14�o�C�g�ȓ��ł��邱��)
	if (strlen(argv[1]) > 14){
		cob_runtime_error( " Error C [%02d-%02d] %s : Option is too long", 00, 02, local_server_time(strTime));
		exit(EXIT_FAILURE);
	}

	//����������(�����I�[�o���CID)����t�@�C�����AID�����p������Apdf�����擾
	PR_edtFileID(argv[1]);

	//�����I�[�o���C�t�@�C���̑��݃`�F�b�N
	ret = chkFileExist(PR_overlayfile);
	if (ret != 0){
		cob_runtime_error(" Error C [%02d-%02d] %s : Overlay File is not exist[%s]", 00, 02, local_server_time(strTime), PR_overlayfile);
		exit(EXIT_FAILURE);
	}

	printf(">> Overlay PDF maker program start \n");

	//�����I�[�o���Cpdf�t�@�C���쐬�̏�������
	ret = PR_Initialize();
#ifdef DEBUG
printf(">> main     PR_Initialize ret : %d\n" , ret);
#endif
	if (ret != 0){
		exit(EXIT_FAILURE);
	}
#ifdef DEBUG
printf(">> main     PR_basename : %s\n" , PR_basename);
#endif

	//�����I�[�o���Cpdf�t�@�C�����J��(pdf�t�@�C���Ƃ��č쐬)
	ret = PR_Open();
	if (ret != 0){
		exit(EXIT_FAILURE);
	}

	//�����I�[�o���Cpdf�t�@�C���̓��e�\���̂��珑���I�[�o���Cpdf�֌��o���l�E�r�����o�͂���
	ret = PR_Write();
	if (ret != 0){
		exit(EXIT_FAILURE);
	}

	//�����I�[�o���Cpdf�t�@�C�������(�ҏW����pdf�t�@�C������������)
	ret = PR_Close();
	if (ret != 0){
		exit(EXIT_FAILURE);
	}

	cob_runtime_error(">> Overlay PDF maker program stop (status : %d) ", ret);

	return ret;
}

//�y�ݒ���擾���āA�����I�[�o���C�t�@�C���������ǂݍ��ݏ����I�[�o���Cpdf�t�@�C���쐬�p�̍\���̂ɒl��ҏW�z
//�@(�Ԓl)(0=����A�ȊO=���̊֐�����̕Ԓl)
int PR_Initialize(){
	float pHeight = 0;		//�p����
	float pWidth = 0;		//�p����
	float pcmp = 0;
	int ret = 0;
	
	OVERLAY_DATA loverlay_data;		//�����I�[�o���C�t�@�C���̓��e(�\���̎���)
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Initialize :");
	gfuncid = 1;
	gtermid = 0;
	
	//xml�ݒ�̓ǂݍ���(�O���[�o���ϐ��֊i�[)
	ret = PR_conf_read();
	if (ret != 0){
		return ret;
	}
	
	//db(M_FORM�e�[�u��)���璠�[�f�[�^���擾
	ret = PR_setProperty();
	if (ret != 0){
		return ret;
	}

	//�ʏ�̉���(�g�嗦)
	PR_horizontalScale = HPDF_Page_GetHorizontalScalling(PR_page);
	
	//�����I�[�o���Cpdf�t�@�C���������쐬
	ret = PR_putInitoverlaypdf();
	if (ret != 0){
		return ret;
	}

	//�����I�[�o���C�t�@�C����ǂݍ���ō\���̂֊i�[
	ret = PR_getoverlaydata(&loverlay_data);
	if (ret != 0){
		return ret;
	}

	//�����I�[�o���C�t�@�C���̓��e(�\����)�̌��o���l�v�f���J�n�ʒu�E�s�A�����L�[�Ƃ��ď����ɕ��בւ�
	PR_sortoverlaydata(&loverlay_data);

	//SD��`����p�����E�����擾���āA���̒l����y�[�W�T�C�Y�E����������
	pHeight = (float)getNum(loverlay_data.sdd, 16, 3) / 10;
	pWidth = (float)getNum(loverlay_data.sdd, 19, 3) / 10;
#ifdef DEBUG
printf(">> PR_Initialize  pHeight : %f\n" , pHeight);
printf(">> PR_Initialize  pWidth  : %f\n" , pWidth);
#endif
#ifdef FORMAT
//M_FORM�Ŏ擾�����p���ݒ���̗p�`SD�w��ɂ��ݒ�����Ȃ�(�f�o�O��)
	//(�p�����E������y�[�W����������)
	if (pHeight > pWidth){
		//(�p���E�c)
		PR_pagestyle = HPDF_PAGE_PORTRAIT;
	}else{
		//(�p���E��)
		PR_pagestyle = HPDF_PAGE_LANDSCAPE;
		//(�y�[�W�T�C�Y����̂��ߏc���l�����ւ�)
		pcmp = pHeight;
		pHeight = pWidth;
		pWidth = pcmp;
	}
	//(�p�����E������y�[�W�T�C�Y������)
	//(A4)
	if (pHeight <= A4_Height && pWidth <= A4_Width){
		PR_pagesize = HPDF_PAGE_SIZE_A4;
	//(B4)
	}else if (pHeight <= B4_Height && pWidth <= B4_Width){
		PR_pagesize = HPDF_PAGE_SIZE_B4;
	//(B4�ȏ�Ȃ�A3�Ƃ���)
	}else{
		PR_pagesize = HPDF_PAGE_SIZE_A3;
//#ifdef DEBUG
//A3�T�C�Y�ł�A4�ɂ���(�f�o�O��)
//PR_pagesize = HPDF_PAGE_SIZE_A4;
//#endif
	}
#endif

	//�����I�[�o���C�t�@�C���̓��e���i�[�����\���̂���I�[�o���Cpdf�t�@�C���쐬�p�̏����I�[�o���Cpdf�t�@�C���\���̂�ҏW
	ret = PR_edtpdfData(&loverlay_data);
	if (ret != 0){
		return ret;
	}
#ifdef DEBUG
int i = 0;
for (i=0; i < gpdf_data_cnt; i++){
printf(">> gpdf_data[%d] no:%d/bRow:%d/bCol:%d/sizeRow:%d/sizeCol:%d \n",
	   i, gpdf_data[i].no, gpdf_data[i].beginRow, gpdf_data[i].beginCol,
	   gpdf_data[i].sizeRow, gpdf_data[i].sizeCol);
printf(">>               lineStyl:%d/lineVar:%d/eRow:%d/eCol:%d/zoom:%d/ranhaba%d \n",
	   gpdf_data[i].lineStyl, gpdf_data[i].lineVar, 
	   gpdf_data[i].endRow, gpdf_data[i].endCol, gpdf_data[i].zoom, gpdf_data[i].ranhaba);
printf(">>               interval:%d/text:[%s] \n", gpdf_data[i].interval, gpdf_data[i].text);
}
#endif

	return ret;
}

//�ypdf���쐬�z
//�@(�Ԓl)(0=����A-1=�G���[����pdf���̔j���G���[�A-2=pdf�����G���[�A�ȊO=���̊֐�����̕Ԓl)
int PR_Open(){
	char fontFilePath[256];
	const char *fontPath = fontFilePath;
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Open :");
	gfuncid = 2;
	gtermid = 0;
	
	//pdf�̎��̂𐶐�
	pdf = HPDF_New(error_handler, NULL);
	if (setjmp(PR_env)) {
		//�G���[�Ȃ�pdf�̎��̂�j��
		gtermid = 1;
		HPDF_Free(pdf);
		cob_runtime_error(" Error C [%02d-%02d]: pdf free ", gfuncid, gtermid);
		return 1;
	}
	
	//���{��G���R�[�h������
	HPDF_UseJPEncodings(pdf);
	//�G���R�[�h��ݒ�
	HPDF_SetCurrentEncoder(pdf,PR_DEFAULT_ENCODE);  
	//���{�ꂪ�g����t�H���g��ǉ�
	HPDF_UseJPFonts(pdf);
	getFontFilePath(PR_fontname,fontFilePath);
	PR_font = HPDF_LoadTTFontFromFile(pdf, fontPath, HPDF_TRUE);
	
	if (!pdf) {
		//pdf���̂̐������s
		gtermid = 2;
		cob_runtime_error(" Error C [%02d-%02d]: cannot create PdfDoc object", gfuncid, gtermid);
		return 1;
	}
	
	//�y�[�W�𐶐�
	ret = PR_NewPage();
	if (ret != 0){
		return ret;
	}
	
	return ret;
}

//�y�����I�[�o���Cpdf�t�@�C��(�\����)���Q�Ƃ��ăI�[�o���Cpdf�t�@�C�����o�͂���z
//�@(�Ԓl)(0=����A�ȊO=���̊֐�����̕Ԓl)
int PR_Write(){
	HPDF_UINT16 DASH_MODE1[] = {1};				//�_���w��Ŏg�p
	HPDF_UINT16 DASH_MODE2[] = {3, 7};
	HPDF_UINT16 DASH_MODE3[] = {8, 7, 2, 7};	//��_�����w��Ŏg�p
	HPDF_Font definition_font;          //pdf�̃t�H���g
	HPDF_Point basPos;			//�����ʒu
	HPDF_Point curPos;			//(���o���l�o�͌��)���݈ʒu
	HPDF_REAL charWidth = 0;	//������
	HPDF_REAL rcharWidth = 0;	//������(�����Z�o�p)
	HPDF_REAL lineHeight = 0;	//���s�l(�~�Q���s��)
	HPDF_REAL Col = 0;
	HPDF_REAL befCol = 0;
	HPDF_REAL Row = 0;
	HPDF_REAL cmpRow = 0;
	HPDF_REAL rWidth = 0;
	HPDF_REAL rHeight = 0;
	HPDF_REAL wx = 0;
	HPDF_REAL wy = 0;
	float ranhaba = 0;
	float saCol = 0;			//�o�͌��ʒu�̒����p����(�P�ʁF�|�C���g)
	int idx = 0;
	int ret = 0;
	HPDF_STATUS retCode;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Write :");
	gfuncid = 3;
	gtermid = 0;

	//�s�̍������i�[(�󎚍s�ƍs����̍\���ɂȂ��Ă���̂ŁA���ۂ̍s���͂Q�{)
	lineHeight = HPDF_Page_GetTextLeading(PR_page);
	//���������i�[
	charWidth = HPDF_Page_TextWidth(PR_page, " ");
#ifdef DEBUG
//printf(">> PR_Write charWidth:%f/lineHeight:%f\n", charWidth, lineHeight);
#endif

	//�����ʒu���i�[
	basPos = HPDF_Page_GetCurrentTextPos(PR_page);
#ifdef DEBUG
printf(">> PR_Write (fst)basX:%f/basY:%f\n", basPos.x, basPos.y);
#endif

	//�s���o���l�t
	//�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�������Q��
	cmpRow = 0;
	befCol = 0;
	for (idx = 0; idx < gpdf_data_cnt; idx++){
		//���o���l�E�l���Ȃ����̂͏���
		if (strlen(gpdf_data[idx].text) > 0){
			//�s���ς������S���ڂ̏o�͈ʒu(befCol)���N���A
			if (gpdf_data[idx].beginRow != cmpRow){
				befCol = 0;
				cmpRow = gpdf_data[idx].beginRow;
			}
			//�w�肳�ꂽ�ʒu�Ɍ��o���l���o��
			//���ʒu(�|�C���g)���Z�o
			Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
			if (Col < befCol){
				//(���ʒu(�|�C���g)���O�o�͂̒P���ʒu�ɂ�����ꍇ���̒P��̊Ԋu�����֏o��)
				if (gpdf_data[idx].interval > 0){
					Col = befCol + (float)gpdf_data[idx].interval * charWidth;
				}
			}
			//�s�ʒu(�|�C���g)���Z�o
			Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 0, 0);
#ifdef DEBUG
printf(">> PR_Write gpdf_data befCol:%f\n", befCol);
printf(">> PR_Write gpdf_data beginXY[%d] beginCol:%d/beginRow:%d/Col:%f/Row:%f\n", idx, gpdf_data[idx].beginCol, gpdf_data[idx].beginRow, Col, Row);
printf(">> PR_Write gpdf_data text[%d] text : [%s]\n", idx, gpdf_data[idx].text);
#endif
			//�|�C���g(�����̃t�H���g��ύX���Ď��{)�ݒ�
			definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
			retCode = HPDF_Page_SetFontAndSize(PR_page, definition_font, PR_fontsize * gpdf_data[idx].point);
			//�g��ݒ�
			if (gpdf_data[idx].zoom == 0){
				//�g��Ȃ�
				retCode = HPDF_Page_SetHorizontalScalling(PR_page, PR_horizontalScale * 1);
				//�g��w�肪�Ȃ��Ƃ��͏o�͌��ʒu�̍����Ȃ�
				saCol = 0;
			}else if  (gpdf_data[idx].zoom == 1){
				//����(�����t�H���g���Q�{���Đ���������1/2����)
				definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
				retCode = HPDF_Page_SetFontAndSize(PR_page, definition_font, PR_fontsize * 2);
				retCode = HPDF_Page_SetHorizontalScalling(PR_page, PR_horizontalScale * 0.5);
				//�o�͌��ʒu��SetHorizontalScalling�w��ɂ��L�k����������Ȃ�
				//���̂��߃t�H���g���Q�{�ɂ��Ă���̂ŏo�͌��ʒu�̍����Ɍ��̕����񕪂̃}�C�i�X��ݒ�
				saCol = strlen(gpdf_data[idx].text) * charWidth * -1;
			}else if  (gpdf_data[idx].zoom == 2){
				//����(�����t�H���g��1/2���Đ����������Q�{����)
				definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
				retCode = HPDF_Page_SetFontAndSize(PR_page, definition_font, PR_fontsize * gpdf_data[idx].point * 0.5);
				retCode = HPDF_Page_SetHorizontalScalling(PR_page, PR_horizontalScale * 2);
				//�o�͌��ʒu��SetHorizontalScalling�w��ɂ��L�k����������Ȃ�
				//���̂��߃t�H���g��1/2�ɂ��Ă���̂ŏo�͌��ʒu�̍����Ɍ��̕�����1/2�̃v���X��ݒ�
				saCol = strlen(gpdf_data[idx].text) / 2 * charWidth;
			}
			//�����Ԋu��ݒ�
			if (gpdf_data[idx].ranhaba > 0){
				//�����w��̂Ƃ�
				//(�����w��Ώۂ̌��o���l�̃T�C�Y�����߂�`�S�p�����p������)
				unsigned char c1 = gpdf_data[idx].text[0];
				if (ISKANJI(c1)){
					//�S�p
					rcharWidth = charWidth * 2;
				}else{
					//���p
					rcharWidth = charWidth;
				}
				//�|�C���g(�����T�C�Y)�w��l�𔽉f
				rcharWidth *= gpdf_data[idx].point;
				//�����̃T�C�Y�����߂�
				ranhaba = (float)gpdf_data[idx].ranhaba * ranhabaUnit;
				if ((ranhaba - rcharWidth) > 0){
					//�����Ԋu�l�����̂Ƃ�
					retCode = HPDF_Page_SetCharSpace(PR_page, (ranhaba - rcharWidth));
					//SetCharSpace�w��ŕ����Ԃ̋󔒂�ݒ肷��Ɩ��������̌�ɂ������ԋ󔒂������
					//���̂��ߕ����ԋ󔒕����o�͌��ʒu�̍����Ƀ}�C�i�X�ݒ�
					saCol = (ranhaba - rcharWidth) * -1;
				}else{
					//�����Ԋu�l�����̂Ƃ�
					retCode = HPDF_Page_SetCharSpace(PR_page, 0);
				}
			}else if (gpdf_data[idx].spaceCharLen > 0){
				//���o���l�A�����`���t���̌���l���U�w��̂Ƃ�
				retCode = HPDF_Page_SetCharSpace(PR_page, (charWidth * gpdf_data[idx].spaceCharLen));
			}else{
				//�w��Ȃ�
				retCode = HPDF_Page_SetCharSpace(PR_page, 0);
			}
			//���o���l���o��
			retCode = HPDF_Page_TextOut(PR_page, Col, Row, gpdf_data[idx].text);

			//�����ʒu�ɖ߂�(x, y�Е����߂��Ȃ��Ƃ��܂��߂�Ȃ�)
			//(�e�L�X�g���o�͂������݈ʒu���擾)
			curPos = HPDF_Page_GetCurrentTextPos(PR_page);
			//(���ʒu��ۑ��^�����|�C���g�ύX���Ή��`��̌��o���o�͂��猳�̒P��Ԋu�����ɏo�͂���)
			befCol = curPos.x;
			befCol += saCol;	//�o�͌��ʒu�̍��������Z���ďo�͌��ʒu�𒲐�
#ifdef DEBUG
//retCode = HPDF_Page_TextOut(PR_page, befCol, curPos.y, "��");
#endif
#ifdef DEBUG
//curPos = HPDF_Page_GetCurrentTextPos(PR_page);
//printf(">> PR_Write curX/curY(   ) : %f / %f\n", curPos.x, curPos.y);
#endif
			//(y�������ɖ߂�)
			wx = 0;
			wy = basPos.y - curPos.y;
#ifdef DEBUG
//printf("PR_Write wx/wy(   )     : %f / %f\n", wx, wy);
#endif
			retCode = HPDF_Page_MoveTextPos(PR_page, wx, wy);
			//(�e�L�X�g���o�͂������݈ʒu���擾)
			curPos = HPDF_Page_GetCurrentTextPos(PR_page);
#ifdef DEBUG
//curPos = HPDF_Page_GetCurrentTextPos(PR_page);
//printf(">> PR_Write curX/curY(n  ) : %f / %f\n", curPos.x, curPos.y);
#endif
			//(x�������ɖ߂�)
			wx = basPos.x - curPos.x;
			wy = 0;
#ifdef DEBUG
//printf(">> PR_Write wx/wy(n  )     : %f / %f\n", wx, wy);
#endif
			retCode = HPDF_Page_MoveTextPos(PR_page, wx, wy);
#ifdef DEBUG
//curPos = HPDF_Page_GetCurrentTextPos(PR_page);
//printf(">> PR_Write basX/basY(re ) : %f / %f\n", curPos.x, curPos.y);
#endif
		}
	}

#ifdef DEBUG
printf(">> PR_Write before line output : \n");
#endif

	//�`�惂�[�h�Ɉڍs
	HPDF_Page_EndText(PR_page);

	//�s�r���t
	//�����I�[�o���Cpdf�t�@�C���̓��e�̍\���̂������Q��
	for (idx = 0; idx < gpdf_data_cnt; idx++){
		if (gpdf_data[idx].lineStyl != 0){
			//�����ݒ�
			if (gpdf_data[idx].lineVar == 1){
				//(����)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateBold);
			}else if (gpdf_data[idx].lineVar == 2){
				//(�א�)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}else if (gpdf_data[idx].lineVar == 3){
				//(���_��)
				HPDF_Page_SetDash (PR_page, DASH_MODE1, 1, 1);
				HPDF_Page_SetLineWidth (PR_page, rateBold);
			}else if (gpdf_data[idx].lineVar == 4){
				//(�ד_��)
				HPDF_Page_SetDash (PR_page, DASH_MODE1, 1, 1);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}else if (gpdf_data[idx].lineVar == 9){
				//(����_����)
				HPDF_Page_SetDash (PR_page, DASH_MODE3, 4, 0);
				HPDF_Page_SetLineWidth (PR_page, rateBold);
			}else if (gpdf_data[idx].lineVar == 10){
				//(�׈�_����)
				HPDF_Page_SetDash (PR_page, DASH_MODE3, 4, 0);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}else if (gpdf_data[idx].lineVar == 11){
				//(����)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateMed);
			}else if (gpdf_data[idx].lineVar == 12){
				//(���_��)
				HPDF_Page_SetDash (PR_page, DASH_MODE1, 1, 1);
				HPDF_Page_SetLineWidth (PR_page, rateMed);
			}else if (gpdf_data[idx].lineVar == 13){
				//(����_����)
				HPDF_Page_SetDash (PR_page, DASH_MODE3, 4, 0);
				HPDF_Page_SetLineWidth (PR_page, rateMed);
			}else{
				//(�K��ȊO���א�)
				HPDF_Page_SetDash (PR_page, NULL, 0, 0);
				HPDF_Page_SetLineWidth (PR_page, rateThin);
			}
			if (gpdf_data[idx].lineStyl == 1){
				//�c��
				//�J�n���ʒu(�|�C���g)���Z�o
				Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
				//�s�ʒu(�|�C���g)���Z�o
				Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 0);
				//�c���̓J�����̏㕔����������̂łP�s�����
				Row += (lineHeight * 2);
				//�J�n���ʒu��ݒ�
				HPDF_Page_MoveTo(PR_page, Col, Row);
				//�I�����ʒu(�|�C���g)���Z�o
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//�s�ʒu(�|�C���g)���Z�o
				Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				//�c���̓J�����̏㕔����������̂łP�s�����
				Row += (lineHeight * 2);
				HPDF_Page_LineTo(PR_page, Col, Row);
			}else if (gpdf_data[idx].lineStyl == 2){
				//����
				//�J�n���ʒu(�|�C���g)���Z�o
				Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
				//�s�ʒu(�|�C���g)���Z�o
				if (gpdf_data[idx].linePos == 1){
					//�ʒu�������w��
					Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 1);
				}else{
					//�ʒu�������w��łȂ�
					Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 0);
				}
				//�J�n���ʒu��ݒ�
				HPDF_Page_MoveTo(PR_page, Col, Row);
				//�I�����ʒu(�|�C���g)���Z�o
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//�s�ʒu(�|�C���g)���Z�o
				//(�������F1/2�s�����)
				//�s�ʒu(�|�C���g)���Z�o
				if (gpdf_data[idx].linePos == 1){
					//�ʒu�������w��
					Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 1);
				}else{
					//�ʒu�������w��łȂ�
					Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				}
				HPDF_Page_LineTo(PR_page, Col, Row);
			}else if (gpdf_data[idx].lineStyl == 3){
				//�����`
				//�I�����ʒu(�|�C���g)���Z�o
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//�s�ʒu(�|�C���g)���Z�o
				Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				//�����`�̓J�����̏㕔����������̂łP�s�����
				Row += (lineHeight * 2);
				//�����`�̕����Z�o
				rWidth = (float)gpdf_data[idx].sizeCol * charWidth;
				//�����`�̍������Z�o
				rHeight = (float)gpdf_data[idx].sizeRow * (lineHeight * 2);
				HPDF_Page_Rectangle(PR_page, Col, Row, rWidth, rHeight);
			}else if (gpdf_data[idx].lineStyl == 5){
				//�ΐ�
				//�J�n���ʒu(�|�C���g)���Z�o
				Col = getColpos(basPos, gpdf_data[idx].beginCol, charWidth);
				//�s�ʒu(�|�C���g)���Z�o
				Row = getRowpos(basPos, gpdf_data[idx].beginRow, lineHeight, 1, 0);
				//�J�n���ʒu��ݒ�
				HPDF_Page_MoveTo(PR_page, Col, Row);
				//�I�����ʒu(�|�C���g)���Z�o
				Col = getColpos(basPos, gpdf_data[idx].endCol, charWidth);
				//�s�ʒu(�|�C���g)���Z�o
				Row = getRowpos(basPos, gpdf_data[idx].endRow, lineHeight, 1, 0);
				HPDF_Page_LineTo(PR_page, Col, Row);
			}
			HPDF_Page_Stroke(PR_page);
		}
	}
	ret = HPDF_Page_BeginText(PR_page);

	return ret;
}

//�ypdf���o�͂���z
//�@(�Ԓl)(0=����A�ȊO=���̊֐�����̕Ԓl)
int PR_Close(){
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Close :");
	gfuncid = 4;
	gtermid = 0;

	ret = PR_Save();
	if (ret == 1){
		return 1;
	}
	
	return ret;
}

//�y�y�[�W�̐����z
//�@(�Ԓl)(0=����)
int PR_NewPage(){
	HPDF_STATUS retCode;
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_NewPage :");
	gfuncid = 5;
	gtermid = 0;

	//page�I�u�W�F�N�g�̍쐬
	PR_page = HPDF_AddPage(pdf);
	
	//�y�[�W�ݒ�(initialize��xml�ݒ肩��擾�܂��͏����I�[�o���C�t�@�C����SD��`���画�肵������)
	retCode = HPDF_Page_SetSize(PR_page, PR_pagesize, PR_pagestyle);
	
	//�t�H���g�ݒ肾���𕪊�
	retCode = fontSetting();
	
	retCode = HPDF_Page_BeginText(PR_page);
	retCode = HPDF_Page_MoveTextPos(PR_page, PR_leftmargin, HPDF_Page_GetHeight(PR_page) - PR_topmargin);
				mcurpos = HPDF_Page_GetCurrentTextPos(PR_page);
	retCode = HPDF_Page_ShowText(PR_page, "");
	
	//���ɏ������ލs���Z�b�g
	PR_currentLine = 1;

	return ret;
}

//�ypdf���������݁A�ۑ��z
//�@(�Ԓl)(0=����A-1=�ۑ��G���[)
int PR_Save(){
	HPDF_STATUS temp;
	char cmd[256]="";
	struct stat st;
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_Save :");
	gfuncid = 6;
	gtermid = 0;

	if (strcmp(PR_id, "999") != 0){
		//�w��̏����I�[�o���C�ݒ肪���݂����Ƃ�
		HPDF_Page_EndText(PR_page);
		temp = HPDF_SaveToFile(pdf, PR_overlaypdf);

		//����ɕۑ��ł��Ȃ���ΏI��
		if (temp != HPDF_OK){
			return 1;
		}

		/* clean up */
		HPDF_Free(pdf);
	}else{
		//�w��̏����I�[�o���C�ݒ肪���݂��Ȃ������Ƃ�(�W��(id=999)���g�p����)
		//temp�t�@�C�����𐶐�
		strcpy(PR_tempname, PR_temppath);
		strcat(PR_tempname, PR_TMP_FNAME);

		//temp�t�@�C���̐���
		if (strstr(PR_tempname, "XXXXXX") != NULL){
			if(mkstemp(PR_tempname) == -1){
				gtermid = 1;
				cob_runtime_error(" Error C [%02d-%02d]: don't create temp File(%s) ", gfuncid, gtermid, PR_tempname);
				ret = 1;
			}else{
				ret = chmod(PR_tempname, 
				            S_IRUSR | S_IWUSR | S_IXUSR |	//rwx
				            S_IRGRP | S_IWGRP | S_IXGRP |	//rwx
				            S_IROTH | S_IWOTH | S_IXOTH		//rwx
				            );
				if (ret != 0){
					//�쐬����temp�t�@�C�����폜
					remove(PR_tempname);
					
					gtermid = 2;
					cob_runtime_error(" Error C [%02d-%02d]: cannot authorize to temp File(%s) ", gfuncid, gtermid, PR_tempname);
					ret = -1;
				}
			}
		}
		if (ret != 0){
			return ret;
		}

		//��������pdf���e��temp�t�@�C���ɏo��
		HPDF_Page_EndText(PR_page);
		temp = HPDF_SaveToFile(pdf, PR_tempname);

		//����ɕۑ��ł��Ȃ���ΏI��
		if (temp != HPDF_OK){
			return 1;
		}

		/* clean up */
		HPDF_Free(pdf);

		//��������pdf�t�@�C��(temp�t�@�C��)�ɔw�i�ƂȂ�pdf���������ď����I�[�o���Cpdf�t�@�C���ɏo��(pdftk�R�}���h�𔭍s)
		sprintf(cmd, "pdftk %s background %s output %s", PR_tempname, PR_basename, PR_overlaypdf);
		ret = system(cmd);
		if (ret != 0){
			gtermid = 3;
			cob_runtime_error(" Error C [%02d-%02d]: cannot join Overlay PDF File(%s) ", gfuncid, gtermid, PR_overlaypdf);
			return 1;
		}

		//temp�t�@�C�����폜����
		ret = stat(PR_tempname, &st);
		if(ret == 0){
			sprintf(cmd, "rm %s ", PR_tempname);
			ret = system(cmd);
			if (ret != 0){
				gtermid = 4;
				cob_runtime_error(" Error C [%02d-%02d]: cannot delete temp File(%s) ", gfuncid, gtermid, PR_tempname);
				return 1;
			}
		}
	}

	return 0;
}

//�yxml�ݒ�t�@�C���̓ǂݍ��݁z
int PR_conf_read(){
	int i;
	char strConfPath[1024];
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_conf_read :");
	gfuncid = 7;
	gtermid = 0;

	//�t�@�C���l�[�������Ƀ��[�_�|�C���^���쐬
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
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
	for (i = 0; i < size; ++i) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, i);
			if (node->content) {
				//�ݒ�t�@�C������DB�̃z�X�g�����擾
				if (strcmp(node->parent->name,CONF_DB_HOST) == 0){
					strcpy(PR_db_server,node->content);
#ifdef DEBUG
printf(">>           dbHost : %s\n" , PR_db_server);
#endif
				}
				//�ݒ�t�@�C������DB�̃��[�U�����擾
				if (strcmp(node->parent->name,CONF_DB_USER) == 0){
					strcpy(PR_db_user,node->content);
				}
				//�ݒ�t�@�C������DB�̃p�X���[�h���擾
				if (strcmp(node->parent->name,CONF_DB_PASS) == 0){
					strcpy(PR_db_password,node->content);
				}
				//�ݒ�t�@�C������DB�����擾
				if (strcmp(node->parent->name,CONF_DB_NAME) == 0){
					strcpy(PR_db_database,node->content);
				}
				//�ݒ�t�@�C������w�i�̃p�X���擾
				if (strcmp(node->parent->name,PR_BASE_PATH) == 0){
					//��Ō��Ƀt�@�C�������Ȃ�
					strcpy(PR_basepath,node->content);
				}
				if (strcmp(node->parent->name,CONF_DB_PORT) == 0){
					PR_db_port = atoi(node->content);
				}
				//�ݒ�t�@�C������TEMP�f�B���N�g���̃p�X���擾
				if (strcmp(node->parent->name,PR_TEMP_PATH) == 0){
					//��Ō��Ƀt�@�C�������Ȃ�
					strcpy(PR_temppath,node->content);
					strcpy(PR_bindname,node->content);
				}
				//�ݒ�t�@�C������t�@�C�����̍ő���擾
				if (strcmp(node->parent->name,PR_OBJ_MAX) == 0){
					//��Ō��Ƀt�@�C�������Ȃ�
					PR_maxObj = atoi(node->content);
				}
				//�ݒ�t�@�C������y�[�W�̍ő���擾
				if (strcmp(node->parent->name,PR_PAGE_MAX) == 0){
					//��Ō��Ƀt�@�C�������Ȃ�
					PR_maxPage = atoi(node->content);

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
	if (PR_maxPage == 0){
		gtermid = 1;
		cob_runtime_error( " Error C [%02d-%02d] : Pagemax value has not been set", gfuncid, gtermid);
		return 1;
	}
	return 0;
}

//�ydb���[�f�[�^��ǂݍ���Őݒ�l��ݒ�z
//�@(�Ԓl)(0=����A1=DB�ڑ��G���[�A-1=query���s�G���[�A-2=pdf���擾�s��)
int PR_setProperty(){
	MYSQL PR_conn, *PR_mysql = &PR_conn;
	MYSQL_RES *res;
	MYSQL_ROW row;
	char query[512] = "";	//�����ݒ��ǂݍ���SQL�̊i�[
	char temp[256] = "";	//sql�ptemporary
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_setProperty : searchID %s ", PR_overlaysearch);
	gfuncid = 8;
	gtermid = 0;

	//DB�ڑ�
	if ((PR_mysql = mysql_init(&PR_conn)) == NULL) {
		cob_runtime_error( "%s", mysql_error(PR_mysql));
		return 1;
	}
//	mybacktrace();
	
	//db�ڑ�
	gtermid = 1;
	if (!mysql_real_connect(&PR_conn, PR_db_server, PR_db_user, PR_db_password, PR_db_database, PR_db_port, NULL, 0)) {  
		cob_runtime_error( " Error C [%02d-%02d]:db_connect server :%s user:%s pass:%s database:%s ", gfuncid, gtermid, PR_db_server,PR_db_user,PR_db_password,PR_db_database);
		return 1;
	}

	//�����ڂƍs�Y�����̑Δ�ꗗ
	// size=row[0]
	// page_style=row[1]
	// font=row[2]
	// font_size=row[3]
	// base_pdf=row[4]
	// line_pitch=row[5]
	// char_pitch=row[6]
	// top_margin=row[7]
	// left_margin=row[8]
	// id=row[9]
	strcat(temp," SELECT mf.size, mf.page_style, mf.font ");
	strcat(temp," , mf.font_size, mf.base_pdf, mf.line_pitch,mf.char_pitch ");
	strcat(temp," , mf.top_margin, mf.left_margin, mf.id ");
	strcat(temp," FROM M_FORM mf ");
	strcat(temp," WHERE id like '%s' or ");
//	strcat(temp," WHERE id = '999-%s' or ");	//'999-overlayfileid'�̌`���̂݉Ƃ���
	strcat(temp,"       id = '999' ");
	strcat(temp," ORDER BY id DESC");
	sprintf(query, temp, PR_overlaysearch);
#ifdef DEBUG
printf(">> PR_setProperty     query : %s\n" , query);
#endif
	
//	printf("%s %s \n",query,PR_db_database);
//	printf("%s \n",query);
	//DB���[�f�[�^�𒊏o
	if (mysql_query(PR_mysql, query)!=0) {
		fprintf(stderr, "%s\n", mysql_error(PR_mysql));
		return 1;
	}
	
	res = mysql_store_result(PR_mysql);
	
	//�����I�[�o���C�t�@�C��ID���܂�id������΂����炩��A�Ȃ���ΕW��(id='999')����擾
	while (row = mysql_fetch_row(res) ){ 
		//�y�[�W�T�C�Y�̎擾
		if (strcmp(row[0],"A4") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_A4;
		}else if (strcmp(row[0],"A5") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_A5;
		}else if (strcmp(row[0],"B4") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_B4;
		}else if (strcmp(row[0],"B5") == 0){
			PR_pagesize = HPDF_PAGE_SIZE_B5;
		}
#ifdef DEBUG
printf(">> PR_setProperty pagesize : %s\n" , row[0]);
#endif
		
		//�y�[�W�̃X�^�C�����擾(�c��)
		if (strcmp(row[1],"HPDF_PAGE_LANDSCAPE") == 0){
			//������
			PR_pagestyle=HPDF_PAGE_LANDSCAPE;
		}else if (strcmp(row[1],"HPDF_PAGE_PORTRAIT") == 0){
			//�c����
			PR_pagestyle=HPDF_PAGE_PORTRAIT;
		}
		
		//�擾���Ă����ݒ���N���A
		memset(PR_fontname,'\0',strlen(PR_fontname));
		//�t�H���g�����i�[
		strcpy(PR_fontname,row[2]);
		//�����񁨐����Ńt�H���g�T�C�Y���i�[
		PR_fontsize = atof(row[3]);
		
		//�擾���Ă����ݒ���N���A
		memset(PR_basename,'\0',strlen(PR_basename));
		//�w�i�ƂȂ�pdf�̃p�X�Ɩ��O���i�[
		strcat(PR_basename,PR_basepath);
		strcat(PR_basename,row[4]);
		//�y�[�W�̍s�����i�[
		PR_linepitch = atof(row[5]);
		//�s�̕��������i�[
		PR_charpitch = atof(row[6]);
#ifdef DEBUG
printf(">> PR_setProperty (string)PR_linepitch/PR_charpitch(string) : %s / %s\n" , row[5], row[6]);
printf(">> PR_setProperty  (float)PR_linepitch/PR_charpitch : %f / %f\n" , atof(row[5]), atof(row[6]));
printf(">> PR_setProperty         PR_linepitch/PR_charpitch : %f / %f\n" , PR_linepitch, PR_charpitch);
#endif
		
		//�㉺�̗]�����i�[
		PR_topmargin = atof(row[7]);
		//���E�̗]�����i�[
		PR_leftmargin = atof(row[8]);
		//ID���i�[
		memset(PR_id, '\0', sizeof(PR_id));
		strcpy(PR_id, row[9]);
		
		break;
	}
	mysql_free_result(res);
	mysql_close(PR_mysql);
	
	//���s������G���[��Ԃ�
	if (strlen(PR_basename) == 0){
		gtermid = 2;
		cob_runtime_error( " Error C [%02d-%02d]: cannot read database for PDF ", gfuncid, gtermid);
		return 1;
	}
	
	return 0;
}

//�y�����I�[�o���Cpdf�t�@�C���̏����쐬�z
//�@(�Ԓl)(0=����ɍ쐬�A-1=�t�@�C���쐬���s�A-2=�����ݒ莸�s)
int PR_putInitoverlaypdf(){
	FILE *fp;
	char cmd[256]="";
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_putInitoverlaypdf :");
	gfuncid = 9;
	gtermid = 0;

	//�����I�[�o���Cpdf�t�@�C������ō쐬(touch�R�}���h�𔭍s)
	sprintf(cmd, "touch %s ", PR_overlaypdf);
	ret = system(cmd);
	if (ret != 0){
		gtermid = 1;
		cob_runtime_error( " Error C [%02d-%02d]: cannot create Overlay PDF File(%s) ", gfuncid, gtermid, PR_overlaypdf);
		return 1;
	}
	
	//�쐬���������I�[�o���Cpdf�t�@�C���Ɍ�����ݒ�
	//(�����w��=rw-rw-rw-)
	ret = chmod(PR_overlaypdf, 
	            S_IRUSR | S_IWUSR |		//rw-
	            S_IRGRP | S_IWGRP |		//rw-
	            S_IROTH | S_IWOTH		//rw-
	            );
	if (ret != 0){
		//�쐬���������I�[�o���Cpdf�t�@�C�����폜
		remove(PR_overlaypdf);
		
		gtermid = 2;
		cob_runtime_error( " Error C [%02d-%02d]: cannot authorize to Overlay PDF File(%s) ", gfuncid, gtermid, PR_overlaypdf);
		return 1;
	}
	
	return 0;
}

//�y�����I�[�o���C�t�@�C���̓ǂݍ��݂ƍ\���̂ւ̊i�[�z
//��OVERLAY_DATA *loverlay_data�F�����I�[�o���C�t�@�C���̓��e(�\����)(I)
//�@(�Ԓl)(0=����A-1=�����I�[�o���C�t�@�C�����J���Ȃ�)
int PR_getoverlaydata(OVERLAY_DATA *loverlay_data){
	FILE *fp;
	char sbuf[OVERLAY_REC_LEN - 1];
	char *cbuf;		//���o���l������
	int cap_len = 0;
	int idx = 0;
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_getoverlaydata :");
	gfuncid = 10;
	gtermid = 0;
	
	//�����I�[�o���C�t�@�C����ǂݍ���
	fp = fopen(PR_overlayfile, "r");
	if (fp != NULL){
		idx = 0;
		while (fgets(sbuf, OVERLAY_REC_LEN, fp) != NULL){
			//���s�R�[�h���폜
			if (strchr(sbuf,'\r') != NULL ){
				*strchr(sbuf,'\r') = '\0';
			}else if(strchr(sbuf,'\n') != NULL ){
				*strchr(sbuf,'\n') = '\0';
			}
#ifdef DEBUG
//printf(">> PR_getoverlaydata     sbuf : %s\n" , sbuf);
#endif
			if (strcmp(getStr(sbuf, 6, 2), "FD") == 0){
				//FD��`
				strcpy(loverlay_data->fdd, getStr(sbuf, 1, FDD_LEN));
			}else if (strcmp(getStr(sbuf, 6, 2), "SD") == 0){
				//SD��`
				strcpy(loverlay_data->sdd, getStr(sbuf, 1, SDD_LEN));
			}else if (strcmp(getStr(sbuf, 6, 2), "IT") == 0 || strcmp(getStr(sbuf, 6, 2), "  ") == 0){
				//IT��`(�󔒂͏ȗ��Ȃ̂�IT�Ƃ݂Ȃ�)
				if (strcmp(getStr(sbuf, 10, 1), "-") == 0){
					//���o���l�̌p��
					cbuf = getStr(sbuf, 11, CONTINUE_LEN);
					if (cap_len + strlen(cbuf) > text_len){
						gtermid = 1;
						cob_runtime_error( " Error C [%02d-%02d]: Caption length over length=%s (%s)", gfuncid, gtermid, text_len, sbuf);
						ret = 1;
						break;
					}
					cap_len += strlen(cbuf);
					//�p���̌��o���l��A��
					strcat(loverlay_data->itd[idx - 1], cbuf);
				}else{
					idx += 1;
					if (idx > GOVERLAY_DATA_MAX){
						gtermid = 2;
						fprintf(stderr," Error C [%02d-%02d]: Overlay File structure table overflow (%s)\n", gfuncid, gtermid, sbuf);
						ret = 1;
						break;
					}
#ifdef DEBUG
printf(">> PR_getoverlaydata sbuf / sbuf length : [%s] / %d\n", sbuf, strlen(sbuf));
#endif
					strcpy(loverlay_data->itd[idx - 1], getStr(sbuf, 1, ITD_LEN));
					cbuf = getStr(sbuf, 49, CAPTION_LEN);
					//���o���l�̌�����ۑ�
					cap_len = strlen(cbuf);
#ifdef DEBUG
printf(">> PR_getoverlaydata loverlay_data->itd[%d] length : %d\n" , idx - 1, strlen(loverlay_data->itd[idx - 1]));
#endif
				}
			}
		}
		//IT��`�̌������i�[
		loverlay_data->itd_cnt = idx;
#ifdef DEBUG
//printf(">> PR_getoverlaydata loverlay_data->itd_cnt : %d\n" , loverlay_data->itd_cnt);
#endif
	}else{
		gtermid = 3;
		fprintf(stderr," Error C [%02d-%02d]: cannot open Overlay File(%s) \n", gfuncid, gtermid, PR_overlayfile);
		ret = -1;
	}
	fclose(fp);
#ifdef DEBUG
//printf("fdd : %s\n" , loverlay_data->fdd);
//printf("sdd : %s\n" , loverlay_data->sdd);
//for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
//printf("itd : %s\n" , loverlay_data->itd[idx]);
//}
#endif
	
	return ret;
}

//�y�����I�[�o���C��`�t�@�C������쐬���������I�[�o���C�t�@�C���̓��e�\���̂������Q�Ƃ���
//�@�����I�[�o���Cpdf�t�@�C���̓��e�\���̂Ƀf�[�^���쐬����z
//��OVERLAY_DATA *loverlay_data�F�����I�[�o���C�t�@�C���̓��e(�\����)(I)
//�@(�Ԓl)(0=����A�ȊO=���̊֐�����̕Ԓl)
int PR_edtpdfData(OVERLAY_DATA *loverlay_data){
	int level = 0;		//�����I�[�o���C�t�@�C���E��`IT�s�̃��x��
	int pdfCnt = 0;		//�����I�[�o���Cpdf�t�@�C���̕ҏW�s��(-1����ΓY���ƂȂ�)
	int boxF = 0;		//���o���ɒ����`�̎w�肪�t���i0=�t�����Ă��Ȃ��A1=�t�����Ă���j
	int befRow = 0;		//�s���ς�������`�F�b�N�p�̕ϐ�(�s�l��ۑ��A����̓[��)
	int chkF = 0;
	int idx = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfData :");
	gfuncid = 11;
	gtermid = 0;
	
	//�����I�[�o���C�t�@�C���̓��e���i�[�����\���̂�IT��`�������Q��
	pdfCnt = 0;
	befRow = 0;
	//IT��`�E�r���̃��x�����e(�\���̎���)������
	clrlevel_data();
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
		//���`�������`������(���o���l�t���̒����`�𔻒�)
		if (strcmp(getStr(loverlay_data->itd[idx], 33, 1), "B") == 0){
			boxF = 1;
		}else{
			boxF = 0;
		}
		//�r���E���`�A���o���l�E�^�C�v�ʂɏ����I�[�o���Cpdf�t�@�C����ҏW
		if (strcmp(getStr(loverlay_data->itd[idx], 36, 1), "A") == 0 ||
			strcmp(getStr(loverlay_data->itd[idx], 36, 1), "N") == 0 ||
			strcmp(getStr(loverlay_data->itd[idx], 36, 1), "X") == 0 ||
			strcmp(getStr(loverlay_data->itd[idx], 36, 1), "K") == 0){
#ifdef DEBUG
printf(">> PR_edtpdfData type is %s\n", getStr(loverlay_data->itd[idx], 36, 1));
#endif
			//���o���l�E�^�C�v�F�p���J�i�A���{��A���{��J�^�J�i�萔�A:���{��P�U�i�萔 �� ���o���l�Ɣ���
			ret = PR_edtpdfDataText(loverlay_data, idx, boxF, &befRow, &pdfCnt);
			if (ret != 0){
				return ret;
			}
			//�����`�w�肪�t�����Ă�����r������������
			if (boxF == 1){
				ret = PR_edtpdfDataLine(loverlay_data, idx, &pdfCnt);
				if (ret != 0){
					return ret;
				}
			}
		}else{
			//���o���l�E�^�C�v�F�l���Ȃ� �� �r���Ɣ���
			chkF = 0;
			if (getNum(loverlay_data->itd[idx], 8, 2) == 01 && strcmp(getStr(loverlay_data->itd[idx], 33, 1), "B") == 0){
				//���x��=01�ŁA���`=�����`
				if (getNum(loverlay_data->itd[idx], 23, 2) != 0 || getNum(loverlay_data->itd[idx], 27, 2) != 0){
					//���x��=01�A���`=�����`�ŌJ��Ԃ��w�肪����Ƃ������ʎw����S�ČJ��Ԃ����ɓ���
					ret = PR_edtpdfDataLineLoop(loverlay_data, idx, &pdfCnt);
					if (ret != 0){
						return ret;
					}
					chkF = 1;
				}
			}
			if (chkF == 0){
				//��L�ȊO(�ʏ킨��ђP�̌J��Ԃ�)
				ret = PR_edtpdfDataLine(loverlay_data, idx, &pdfCnt);
				if (ret != 0){
					return ret;
				}
			}
		}
#ifdef DEBUG
//printf("PR_edtpdfData itd[%d]/itd length : [%s] / %d\n", idx, getStr(loverlay_data->itd[idx], 11, 46), strlen(getStr(loverlay_data->itd[idx], 11, 46)));
#endif
	}

	//�����I�[�o���Cpdf�t�@�C���̓��e�̍s�A���̒������s��
	ret = setpdfDataColRow(&pdfCnt);

	//�����I�[�o���Cpdf�t�@�C���̓��e�̌������i�[
	gpdf_data_cnt = pdfCnt;

#ifdef DEBUG
for (idx = 0; idx < level_data_len; idx++){
printf(">> PR_edtpdfData - glevel_data[%d].levelNo/beginRow/beginCol  : %d / %d / %d\n" , idx, glevel_data[idx].levelNo, glevel_data[idx].beginRow, glevel_data[idx].beginCol);
printf(">> PR_edtpdfData - glevel_data[%d].sizeRow/sizeCol  : %d / %d\n" , idx, glevel_data[idx].sizeRow, glevel_data[idx].sizeCol);
}
#endif

	return ret;
}

//�y�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�Ɍ��o���l�f�[�^���쐬�z
//��OVERLAY_DATA *loverlay_data�F�����I�[�o���C�t�@�C���̓��e(�\����)(I)
//�@int pidx                   �F�����I�[�o���C�t�@�C���̓��e(�\����)�J�����g�̓Y����(I)
//�@int boxF                   �F���o���l�ɒ����`�w�肪�t�����Ă��邩�t���O(I)
//�@int *befRow                �F�����I�[�o���C�t�@�C���̓��e(�\����)�̂P�O�̗v�f�̍s�ʒu(I-O)
//�@int pidx                   �F�����I�[�o���C�t�@�C���̓��e(�\����)�J�����g�̓Y����(I)
//�@(�Ԓl)(0=����A�ȊO=���̊֐�����̕Ԓl)
int PR_edtpdfDataText(OVERLAY_DATA *loverlay_data, int pidx, int boxF, int *befRow, int *cnt){
	char *getbuf;
	char buf[351];
	char *wbuf;
	unsigned char cnvbuf[701];	//���p���S�p�ϊ��G���A
	unsigned int cnvint = 0;	//�P�U�i�����{��ϊ��p
	int beginRow = 0;
	int beginCol = 0;
	int sizeRow = 0;
	int sizeCol = 0;
	int rowPos = 0;
	int colPos = 0;
	int edtRow = 0;
	int edtCol = 0;
	int curRow = 0;
	int xrep = 0;		//�������J��Ԃ���
	int yrep = 0;		//�c�����J��Ԃ���
	int xival = 0;		//�������J��Ԃ��̌��Ԋu
	int yival = 0;		//�c�����J��Ԃ��̍s�Ԋu
	int charCnt = 0;
	int curidx = 0;
	int idx = 0;
	int xidx = 0;
	int yidx = 0;
	int pos = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfDataText :");
	gfuncid = 12;
	gtermid = 0;

	beginRow = getNum(loverlay_data->itd[pidx], 11, 3);	//�J�n�ʒu�E�s
	beginCol = getNum(loverlay_data->itd[pidx], 14, 3);	//�J�n�ʒu�E��

	//���o���l(�󎚂���e�L�X�g)��ҏW
	//(���o���l(�󎚂���e�L�X�g)��؂�o��)
	getbuf = getStr(loverlay_data->itd[pidx], 49, text_len);
	strcpy(buf, getbuf);
	RTrim(buf);	//�p���͘A���ς݂Ȃ̂Ŗ����̋󔒂͎�苎��
	//(�������Z)
	ret = addpdf_dataCnt(cnt);
#ifdef DEBUG
printf(">> PR_edtpdfDataText - cnt/buf/buf length : %d / [%s] / %d\n", *cnt, buf, strlen(buf));
#endif
	if (ret != 0){
		return ret;
	}
	clrpdf_data(*cnt - 1);	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���N���A
	gpdf_data[*cnt - 1].no = *cnt;		//�A��
#ifdef DEBUG
printf(">> PR_edtpdfDataText boxF : %d\n", boxF);
#endif
	if (boxF == 0){
		//�ʏ�̌��o���w��
		gpdf_data[*cnt - 1].beginRow = beginRow;	//�J�n�ʒu�E�s
		gpdf_data[*cnt - 1].beginCol = beginCol;	//�J�n�ʒu�E��
	}else{
		//���o���ɒ����`�w�肪�t��
		//(�傫���E�s�A���̒l���擾)
		sizeRow = getNum(loverlay_data->itd[pidx], 17, 3);	//�傫���E�s
		sizeCol = getNum(loverlay_data->itd[pidx], 20, 3);	//�傫���E��
		//(�z�u�E�s�ʒu�w�肩�猩�o���l�̏o�͍s��ݒ�)
		if (strcmp(getStr(loverlay_data->itd[pidx], 44, 2), " U") == 0){
			//(�ŏ㕔)
			gpdf_data[*cnt - 1].beginRow = beginRow;
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 44, 2), " C") == 0){
			//(����)
			gpdf_data[*cnt - 1].beginRow = beginRow + (sizeRow / 2);
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 44, 2), " D") == 0){
			//(�ŉ���)
			gpdf_data[*cnt - 1].beginRow = beginRow + sizeRow - 1;
		}else{
			//(���l�ōs�w��)
			edtRow = getNum(loverlay_data->itd[pidx], 44, 2);
			if (edtRow < 1 || edtRow > (beginRow + sizeRow - 1)){
				//�s�w�肪�P�ȉ��܂��͒����`�̍ŉ����������Ă���Ƃ��͍ŏ㕔�Ƃ���
				edtRow = 1;
			}
			gpdf_data[*cnt - 1].beginRow = beginRow + edtRow - 1;
		}
		//(�z�u�E���ʒu�w�肩�猩�o���l�̏o�͌���ݒ�)
		if (strcmp(getStr(loverlay_data->itd[pidx], 46, 2), " L") == 0){
			//(���l��)
			gpdf_data[*cnt - 1].beginCol = beginCol;
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 46, 2), " R") == 0){
			//(�E�l��)
			gpdf_data[*cnt - 1].beginCol = beginCol + sizeCol - strlen(buf);
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 46, 2), " D") == 0){
			//(��l���U)
			//(���o���l�̕����񐔂��J�E���g�A�o�͂��钷���`�̉����ɋϓ��ɂȂ�悤�ɕ����Ԃ̕��������v�Z)
			charCnt = getCharCnt(buf);
			gpdf_data[*cnt - 1].spaceCharLen = ((float)sizeCol - (float)strlen(buf)) / ((float)charCnt + 1);
			//(���o���l�̑O�ɔ��p�󔒂�t�����ĂP���O���o�͈ʒu�Ƃ���(�ϓ��o�͂̂��߂̋󔒂��o�͂��邽��))
			wbuf = (char *)malloc(strlen(buf) + 1);
			memset(wbuf, '\0', charCnt + 1);
			sprintf(wbuf, " %s", buf);
			strcpy(buf, wbuf);
			free(wbuf);
			gpdf_data[*cnt - 1].beginCol = beginCol - 1;
		}else{
			//(���l�Ō��w��)
			edtCol = getNum(loverlay_data->itd[pidx], 46, 2);
			if (edtCol < 1 || edtCol > (beginCol + sizeCol - strlen(buf) - 1)){
				//���w�肪�P�ȉ��܂��͒����`�̉E�l�߂Ɏ��܂�Ȃ��Ƃ��͍��l�߂Ƃ���
				edtCol = 1;
			}
			gpdf_data[*cnt - 1].beginCol = beginCol + edtCol - 1;
		}
	}
	//�|�C���g
	if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "8") == 0){
		gpdf_data[*cnt - 1].point = 0.8;		//(80%����)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "9") == 0){
		gpdf_data[*cnt - 1].point = 0.9;		//(90%����)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Y") == 0){
		gpdf_data[*cnt - 1].point = 1.5;		//(150%����)
		gpdf_data[*cnt - 1].beginRow += 1;		//(�g�啶���͊g�啪��s�ɏo�͂����̂ŊJ�n�s��+1����)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Z") == 0){
		gpdf_data[*cnt - 1].point = 2.0;		//(200%����)
		gpdf_data[*cnt - 1].beginRow += 1;		//(�g�啶���͊g�啪��s�ɏo�͂����̂ŊJ�n�s��+1����)
	}else{
		gpdf_data[*cnt - 1].point = 1.0;		//(100%����)
	}
	//�g��
	if (strcmp(getStr(loverlay_data->itd[pidx], 40, 1), "T") == 0){
		gpdf_data[*cnt - 1].zoom = 1;			//(����)
		if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Y") != 0 &&
			strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Z") != 0){
			//�g�啶���͏���
			gpdf_data[*cnt - 1].beginRow += 1;		//(���̂͏c�ɒ����Ȃ������A��s�ɏo�͂����̂ŊJ�n�s��+1����)
		}
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 40, 1), "F") == 0){
		gpdf_data[*cnt - 1].zoom = 2;			//(����)
		//(���̂͏c�̒���1/2����̂Ŋg�啶��(150, 200%����)���Ώۂ̂Ƃ��A�J�n�s��+1���ꂽ����-1����)
		if (strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Y") == 0 ||
			strcmp(getStr(loverlay_data->itd[pidx], 39, 1), "Z") == 0){
			gpdf_data[*cnt - 1].beginRow -= 1;
		}
	}else{
		gpdf_data[*cnt - 1].zoom = 0;			//(�Ȃ�)
	}
	gpdf_data[*cnt - 1].ranhaba = getNum(loverlay_data->itd[pidx], 41, 2);	//����

	//����s�̒P��Ԃ̃o�C�g�����擾�`�ݒ�(�擪�̒P��ɂ̓[����ҏW)
	if (gpdf_data[*cnt - 1].beginRow != *befRow){
		gpdf_data[*cnt - 1].interval = 0;
		*befRow = gpdf_data[*cnt - 1].beginRow;
	}else{
		//�P�O�̍��ڂ����Ȃ�[����ҏW
		if (gpdf_data[*cnt - 1 - 1].lineStyl != 0){
			gpdf_data[*cnt - 1].interval = 0;
		}else{
			//�P�O�̍��ڂ̊J�n�ʒu�E���A���o���l�̃o�C�g���A���݂̍��ڂ̊J�n�ʒu�E������P��Ԃ̃o�C�g�����Z�o
			//(���A�J�n�ʒu�E�� - (�O�A�J�n�ʒu�E�� + �O�A���o���l�̃o�C�g��))
			gpdf_data[*cnt - 1].interval = gpdf_data[*cnt - 1].beginCol - (gpdf_data[*cnt - 1 - 1].beginCol + strlen(gpdf_data[*cnt - 1 - 1].text));
		}
	}

	//���{��P�U�i�萔�̎w��̂Ƃ����{��ɕϊ�
	if (strcmp(getStr(loverlay_data->itd[pidx], 36, 1), "X") == 0){
		wbuf = (char *)malloc(strlen(buf));
		memset(wbuf, '\0', strlen(buf));
		strcpy(wbuf, buf);
		//���{��P�U�i�萔�̎w��
		pos = 0;
		for (idx = 0; idx < strlen(wbuf) && wbuf[idx] != '\0'; idx++){
			if (wbuf[idx] == '\0'){
				break;
			}
			sscanf(&wbuf[idx], "%02x", &cnvint);
			buf[pos] = cnvint;
			idx += 1;	//�P�o�C�g�����Z
			pos += 1;	//�ҏW�ʒu���Z
		}
		buf[pos] = '\0';
#ifdef DEBUG
//printf(">> PR_edtpdfDataText     str : [%s]\n", wbuf);
#endif
		free(wbuf);
	}

	//���{��J�^�J�i�萔�̎w��̂Ƃ����p����S�p�ɕϊ�
	if (strcmp(getStr(loverlay_data->itd[pidx], 36, 1), "K") == 0){
		cnvHanToZen(buf, cnvbuf);
		strcpy(buf, cnvbuf);
	}

	//���o���l�̕ҏW
	curidx = *cnt - 1;	//���݂̌��o���l�v�f(�J��Ԃ��̂Ƃ��̊�v�f)�̓Y������ۑ�
	ret = edtpdfText(loverlay_data, pidx, cnt, buf, &curRow);

	//�J��Ԃ�(�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�֌J��Ԃ����̗v�f���쐬����)
	yrep = getNum(loverlay_data->itd[pidx], 23, 2);	//(�c�����J��Ԃ���)
	if (yrep > 0){
		yival = getNum(loverlay_data->itd[pidx], 25, 2);	//�c�����J��Ԃ��s�Ԋu
		if (strcmp(getStr(loverlay_data->itd[pidx], 48, 1), "Y") == 0){
			yival += getCharCnt(buf);	//�c���� �� ���o���l�A�c�����Ȃ̂Ō��̌��o���l�̒��������s�ƂȂ�̂Ō��o���l�̒������Z
		}else{
			yival += 1;					//������ �� ���o���l�A���s���͂P�s�����Z
		}
	}
	xrep = getNum(loverlay_data->itd[pidx], 27, 2);	//(�������J��Ԃ���)
	if (xrep > 0){
		xival = getNum(loverlay_data->itd[pidx], 29, 2);	//�������J��Ԃ����Ԋu
		xival += strlen(gpdf_data[curidx].text);	//���o���l�A�����������Z
		if (yrep <= 0){
			//�������J��Ԃ�����ŁA�c�����J��Ԃ��Ȃ��̂Ƃ��A�R�[�h�に��for���[�v���X���[�����̂ŁA
			//�c�����J��Ԃ��� 1 ��ݒ肵�Ă���(�P��J��Ԃ��Ȃ̂ŉe���͂Ȃ�)
			yrep = 1;
		}
	}
	for (yidx = 0; yidx < yrep; yidx++){
		//�c�����J��Ԃ�
		if (yidx > 0){
			//���݂̌��o���l�v�f(�J��Ԃ��̂Ƃ��̊�v�f)������̂łP�ڂ͗v�f���쐬���Ȃ�
			ret = addpdf_dataCnt(cnt);
			if (ret != 0){
				return ret;
			}
			clrpdf_data(*cnt - 1);	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���N���A
			gpdf_data[*cnt - 1].no = *cnt;		//�A��
			gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//�J�n�ʒu�E�s
			gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol;	//�J�n�ʒu�E��
			gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//�傫���E�s
			gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//�傫���E��
			gpdf_data[*cnt - 1].point = gpdf_data[curidx].point;		//�|�C���g
			gpdf_data[*cnt - 1].zoom = gpdf_data[curidx].zoom;			//�g��
			gpdf_data[*cnt - 1].ranhaba = gpdf_data[curidx].ranhaba;	//����
			gpdf_data[*cnt - 1].interval = gpdf_data[curidx].interval;	//����s�P��Ԃ̃o�C�g��(�擪�̒P��̓[����ݒ�)
			gpdf_data[*cnt - 1].spaceCharLen = gpdf_data[curidx].spaceCharLen;	//�����ԋ󔒁i�P��=������(������)�F���o���l�A�����`�t���p�j
			curRow = gpdf_data[*cnt - 1].beginRow;
			ret = edtpdfText(loverlay_data, pidx, cnt, buf, &curRow);
		}
		for (xidx = 0; xidx < xrep; xidx++){
			//�������J��Ԃ�
			if (xidx > 0){
				//���݂̌r���v�f(�J��Ԃ��̂Ƃ��̊�v�f)������̂łP�ڂ͗v�f���쐬���Ȃ�
				ret = addpdf_dataCnt(cnt);
				if (ret != 0){
					return ret;
				}
				clrpdf_data(*cnt - 1);	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���N���A
				gpdf_data[*cnt - 1].no = *cnt;		//�A��
				gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//�J�n�ʒu�E�s
				gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol + (xival * xidx);	//�J�n�ʒu�E��
				gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//�傫���E�s
				gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//�傫���E��
				gpdf_data[*cnt - 1].point = gpdf_data[curidx].point;		//�|�C���g
				gpdf_data[*cnt - 1].zoom = gpdf_data[curidx].zoom;			//�g��
				gpdf_data[*cnt - 1].ranhaba = gpdf_data[curidx].ranhaba;	//����
				gpdf_data[*cnt - 1].interval = gpdf_data[curidx].interval;	//����s�P��Ԃ̃o�C�g��(�擪�̒P��̓[����ݒ�)
				gpdf_data[*cnt - 1].spaceCharLen = gpdf_data[curidx].spaceCharLen;	//�����ԋ󔒁i�P��=������(������)�F���o���l�A�����`�t���p�j
				curRow = gpdf_data[*cnt - 1].beginRow;
				ret = edtpdfText(loverlay_data, pidx, cnt, buf, &curRow);
			}
		}
	}

	return ret;
}

//�y�����I�[�o���Cpdf�t�@�C���̓��e�\���̂Ɍr���f�[�^���쐬�z
//��OVERLAY_DATA *loverlay_data�F�����I�[�o���C�t�@�C���̓��e(�\����)(I)
//�@int pidx                   �F�����I�[�o���C�t�@�C���̓��e(�\����)�J�����g�̓Y����(I)
//�@int *cnt                   �F�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�̌���(I-O)
//�@(�Ԓl)(0=����A�ȊO=���̊֐�����̕Ԓl)
int PR_edtpdfDataLine(OVERLAY_DATA *loverlay_data, int pidx, int *cnt){
	char *buf;
	int beginRow = 0;
	int beginCol = 0;
	int sizeRow = 0;
	int sizeCol = 0;
	int levelNo = 0;
	int levelF = 0;		//���x���z���i0=�z���ɂȂ����l���s�v�A1=�z���ɂ���j
	int curidx = 0;		//�J��Ԃ����̔z��ʒu
	int repival = 0;	//�J��Ԃ��Ԋu
	int xrep = 0;		//�������J��Ԃ���
	int yrep = 0;		//�c�����J��Ԃ���
	int xival = 0;		//�������J��Ԃ��̌��Ԋu
	int yival = 0;		//�c�����J��Ԃ��̍s�Ԋu
	int idx = 0;
	int xidx = 0;
	int yidx = 0;
	int chkF = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfDataLine :");
	gfuncid = 13;
	gtermid = 0;

	//�J�n�ʒu�A�傫���ƌJ��Ԃ��񐔂́A�ϐ��ɐ؂�o���Ă���
	beginRow = getNum(loverlay_data->itd[pidx], 11, 3);	//�J�n�ʒu�E�s
	beginCol = getNum(loverlay_data->itd[pidx], 14, 3);	//�J�n�ʒu�E��
	sizeRow = getNum(loverlay_data->itd[pidx], 17, 3);	//�傫���E�s
	sizeCol = getNum(loverlay_data->itd[pidx], 20, 3);	//�傫���E��
	yrep = getNum(loverlay_data->itd[pidx], 23, 2);	//�c�����J��Ԃ���
	xrep = getNum(loverlay_data->itd[pidx], 27, 2);	//�������J��Ԃ���

	//(�������Z)
	ret = addpdf_dataCnt(cnt);
	if (ret != 0){
		return ret;
	}
	clrpdf_data(*cnt - 1);	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���N���A

	gpdf_data[*cnt - 1].no = *cnt;			//�A��
#ifdef DEBUG
printf(">> PR_edtpdfDataLine gpdf_data[%d] no : %d\n", *cnt - 1, *cnt);
#endif
	levelNo = getNum(loverlay_data->itd[pidx], 8, 2);
	gpdf_data[*cnt - 1].levelNo = levelNo;	//���x���ԍ�
	//���`
	if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "V") == 0){
		gpdf_data[*cnt - 1].lineStyl = 1;		//(�c��)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "H") == 0){
		gpdf_data[*cnt - 1].lineStyl = 2;		//(����)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "B") == 0){
		gpdf_data[*cnt - 1].lineStyl = 3;		//(�����`)
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 33, 1), "O") == 0){
		gpdf_data[*cnt - 1].lineStyl = 5;		//(�ΐ�)
	}
#ifdef DEBUG
//printf(">> PR_edtpdfDataLine gpdf_data[%d] lineStyle, overLayData_33 : %d / [%s]\n", *cnt - 1, gpdf_data[*cnt - 1].lineStyl, getStr(loverlay_data->itd[pidx], 33, 1));
#endif
	//����
	if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "1") == 0){
		gpdf_data[*cnt - 1].lineVar = 1;	//����
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "2") == 0){
		gpdf_data[*cnt - 1].lineVar = 2;	//�א�
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "3") == 0){
		gpdf_data[*cnt - 1].lineVar = 3;	//���_��
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "4") == 0){
		gpdf_data[*cnt - 1].lineVar = 4;	//�ד_��
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "9") == 0){
		gpdf_data[*cnt - 1].lineVar = 9;	//����_����
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "A") == 0){
		gpdf_data[*cnt - 1].lineVar = 10;	//�׈�_����
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "O") == 0){
		gpdf_data[*cnt - 1].lineVar = 11;	//����
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "P") == 0){
		gpdf_data[*cnt - 1].lineVar = 12;	//���_��
	}else if (strcmp(getStr(loverlay_data->itd[pidx], 34, 1), "S") == 0){
		gpdf_data[*cnt - 1].lineVar = 13;	//����_����
	}

	//���x���ԍ��w�肠��̂Ƃ��A�r���̃��x�����e��ݒ肷��
	//(���x���ԍ��̎w�肪�Ȃ��ꍇ�ƈʒu�w�蓙�̃J�������قȂ�)
	if (levelNo > 0){
		if (levelNo > 10){
			//���x���ԍ���10�ȏ�͏������Ȃ�
			gtermid = 1;
			fprintf(stderr," Error C [%02d-%02d]: level no is 10 over (%s)\n", gfuncid, gtermid, loverlay_data->itd[pidx]);
			return ret;
		}
		idx = levelNo - 1;
		if (levelNo == 1){
			//���x��=01
			//IT��`�E�r���̃��x�����e(�\���̎���)������
			clrlevel_data();
			glevel_data[idx].levelNo = levelNo;		//���x��(�ԍ�)
			glevel_data[idx].lineStyl = gpdf_data[*cnt - 1].lineStyl;	//���`
			glevel_data[idx].beginRow = beginRow;	//�J�n�ʒu�E�s
			glevel_data[idx].beginCol = beginCol;	//�J�n�ʒu�E��
			glevel_data[idx].sizeRow = sizeRow;	//�傫���E�s
			glevel_data[idx].sizeCol = sizeCol;	//�傫���E��
		}else{
			//���x��=02�ȍ~
			if (glevel_data[idx].levelNo == 0 || gpdf_data[*cnt - 1].lineStyl != glevel_data[idx].lineStyl){
				//���x�����e���ݒ肳��Ă��Ȃ��Ƃ��A�܂��͐��`���ς�����Ƃ��̂ݒl��ݒ肷��
				glevel_data[idx].levelNo = levelNo;		//���x��(�ԍ�)
				glevel_data[idx].lineStyl = gpdf_data[*cnt - 1].lineStyl;	//���`
				//�\�`���̔���
				chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
				if (chkF == 1){
					//(�\�`��)
					if (gpdf_data[*cnt - 1].lineStyl == 1){
						//(�c��)
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow;
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol + sizeCol - 1;
					}else if (gpdf_data[*cnt - 1].lineStyl == 2){
						//(����)
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow + sizeRow - 1;
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol;
					}else{
						//(�c�E�����ȊO�͏������Ȃ�)
						gtermid = 2;
						fprintf(stderr," Error C [%02d-%02d]: tabular form's line is not vertical or horizontal (%s)\n", gfuncid, gtermid, loverlay_data->itd[pidx]);
						return ret;
					}
				}else{
					//(�\�`���łȂ�)
					if (beginRow == 0){
						//�J�n�ʒu�E�s�̒l���ݒ肳��Ă��Ȃ��Ƃ���1�O�̃��x���̊J�n�ʒu�E�s�̒l�������p��
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow;
					}else{
						glevel_data[idx].beginRow = glevel_data[idx - 1].beginRow + beginRow - 1;
					}
					if (beginCol == 0){
						//�J�n�ʒu�E���̒l���ݒ肳��Ă��Ȃ��Ƃ���1�O�̃��x���̊J�n�ʒu�E���̒l�������p��
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol;
					}else{
						glevel_data[idx].beginCol = glevel_data[idx - 1].beginCol + beginCol - 1;
					}
				}
				glevel_data[idx].sizeRow = sizeRow;	//�傫���E�s
				glevel_data[idx].sizeCol = sizeCol;	//�傫���E��
			}
		}
	}else{
		//���x���ԍ��w��Ȃ�(=00)�̂Ƃ��AIT��`�E�r���̃��x�����e(�\���̎���)������
		clrlevel_data();
	}

	//�J�n�ʒu
	if (gpdf_data[*cnt - 1].levelNo <= 1){
		//���x��=�Ȃ�, 01
		gpdf_data[*cnt - 1].beginRow = beginRow;
		gpdf_data[*cnt - 1].beginCol = beginCol;
	}else{
		//���x��=02�`
		idx = levelNo - 1;
		//�\�`���̔���
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(�\�`��)
			if (gpdf_data[*cnt - 1].lineStyl == 1){
				//(�c��)
				//(�s�F�e���x���̍s���擾)
				gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow;
				//(���F���x���܂��͐��킪�P�O�̃f�[�^�ƕς�����Ƃ� �� �e���x���̌� + �傫���E�� - �P)
				//(���F���x���܂��͐��킪�P�O�̃f�[�^�Ɠ����Ƃ� �� �P�O�̃f�[�^�̌� + �傫���E��)
				if (gpdf_data[*cnt - 1].levelNo != gpdf_data[*cnt - 1 - 1].levelNo ||
					gpdf_data[*cnt - 1].lineStyl != gpdf_data[*cnt - 1 - 1].lineStyl){
					gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol + sizeCol - 1;
				}else{
					gpdf_data[*cnt - 1].beginCol = gpdf_data[*cnt - 1 - 1].beginCol + sizeCol;
				}
				//�傫���E�s�A���ɒl��ݒ�
				//(�s�F�e���x���̑傫���E�s���擾)
				gpdf_data[*cnt - 1].sizeRow = glevel_data[idx - 1].sizeRow;
				//(���F�e���x���Ƃ̌������������Ă���̂Ń[��)
				gpdf_data[*cnt - 1].sizeCol = 0;
			}else if (gpdf_data[*cnt - 1].lineStyl == 2){
				//(����)
				//(�s�F���x���܂��͐��킪�P�O�̃f�[�^�ƕς�����Ƃ� �� �e���x���̍s + �傫���E�s - �P)
				//(�s�F���x���܂��͐��킪�P�O�̃f�[�^�Ɠ����Ƃ� �� �P�O�̃f�[�^�̍s + �傫���E�s)
				if (gpdf_data[*cnt - 1].levelNo != gpdf_data[*cnt - 1 - 1].levelNo ||
					gpdf_data[*cnt - 1].lineStyl != gpdf_data[*cnt - 1 - 1].lineStyl){
					gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow + sizeRow - 1;
				}else{
					gpdf_data[*cnt - 1].beginRow = gpdf_data[*cnt - 1 - 1].beginRow + sizeRow;
				}
				//(���F�e���x���̌����擾)
				gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol;
				//�傫���E�s�A���ɒl��ݒ�
				//(�s�F�e���x���Ƃ̍s�����������Ă���̂Ń[��)
				gpdf_data[*cnt - 1].sizeRow = 0;
				//(���F�e���x���̑傫���E�����擾)
				gpdf_data[*cnt - 1].sizeCol = glevel_data[idx - 1].sizeCol;
			}else{
				//(�c�E�����ȊO�͏������Ȃ�)
				gtermid = 3;
				fprintf(stderr," Error C [%02d-%02d]: tabular form's line is not vertical or horizontal (%s)\n", gfuncid, gtermid, loverlay_data->itd[pidx]);
				return ret;
			}
		}else{
			//(�\�`���łȂ�)
			if (beginRow == 0){
				//�J�n�ʒu�E�s�̒l���ݒ肳��Ă��Ȃ��Ƃ���1�O�̃��x���̊J�n�ʒu�E�s�̒l�������p��
				gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow;
			}else{
				gpdf_data[*cnt - 1].beginRow = glevel_data[idx - 1].beginRow + beginRow - 1;
			}
			if (beginCol == 0){
				//�J�n�ʒu�E���̒l���ݒ肳��Ă��Ȃ��Ƃ���1�O�̃��x���̊J�n�ʒu�E���̒l�������p��
				gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol;
			}else{
				gpdf_data[*cnt - 1].beginCol = glevel_data[idx - 1].beginCol + beginCol - 1;
			}
		}
	}

	//�傫���E�s�A��
	if (gpdf_data[*cnt - 1].levelNo <= 1){
		//���x��=�Ȃ�, 01
		gpdf_data[*cnt - 1].sizeRow = sizeRow;	//�傫���E�s
		gpdf_data[*cnt - 1].sizeCol = sizeCol;	//�傫���E��
	}else{
		//���x��=02�`
		idx = levelNo - 1;
		//�\�`���̔���
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(�\�`��)
			//(�\�`���͊J�n�ʒu�̒l�ݒ莞�ɐݒ�ς݂̂��߁A�����ł͂��Ȃ�)
		}else{
			//(�\�`���łȂ�)
			if (gpdf_data[*cnt - 1].lineStyl == 1 || gpdf_data[*cnt - 1].lineStyl == 3){
				//(�c���܂��͒����`)
				if (sizeRow == 0){
					//�傫���E�s�̒l���ݒ肳��Ă��Ȃ��Ƃ���1�O�̃��x���̑傫���E�s�̒l�������p��
					gpdf_data[*cnt - 1].sizeRow = glevel_data[idx - 1].sizeRow;
				}else{
					gpdf_data[*cnt - 1].sizeRow = sizeRow;
				}
			}
			if (gpdf_data[*cnt - 1].lineStyl == 2 || gpdf_data[*cnt - 1].lineStyl == 3){
				//(�����܂��͒����`)
				if (sizeCol == 0){
					//�傫���E���̒l���ݒ肳��Ă��Ȃ��Ƃ���1�O�̃��x���̑傫���E���̒l�������p��
					gpdf_data[*cnt - 1].sizeCol = glevel_data[idx - 1].sizeCol;
				}else{
					gpdf_data[*cnt - 1].sizeCol = sizeCol;
				}
			}
		}
	}

	//�I���ʒu
	if (gpdf_data[*cnt - 1].lineStyl == 1){
		//�c��
		gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow;	//�I���ʒu�E�s
		gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol;	//�I���ʒu�E��
	}else if (gpdf_data[*cnt - 1].lineStyl == 2){
		//����
		gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow;	//�I���ʒu�E�s
		gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol + gpdf_data[*cnt - 1].sizeCol;	//�I���ʒu�E��
		if (strcmp(getStr(loverlay_data->itd[pidx], 35, 1), "C") == 0){
			gpdf_data[*cnt - 1].linePos = 1;	//�ʒu
		}
	}else if (gpdf_data[*cnt - 1].lineStyl == 3){
		//�����`
		gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow;	//�I���ʒu�E�s
		gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol;	//�I���ʒu�E��
	}else if (gpdf_data[*cnt - 1].lineStyl == 5){
		//�ΐ�
		if (strcmp(getStr(loverlay_data->itd[pidx], 35, 1), "R") == 0){
			//�E������ΐ�
			gpdf_data[*cnt - 1].beginRow = gpdf_data[*cnt - 1].beginRow - 1;	//�J�n�ʒu�E�s
			gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow;	//�I���ʒu�E�s
			gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol + gpdf_data[*cnt - 1].sizeCol;	//�I���ʒu�E��
		}else if (strcmp(getStr(loverlay_data->itd[pidx], 35, 1), "L") == 0){
			//��������ΐ�
			gpdf_data[*cnt - 1].beginRow = gpdf_data[*cnt - 1].beginRow + gpdf_data[*cnt - 1].sizeRow - 1;	//�J�n�ʒu�E�s
			gpdf_data[*cnt - 1].endRow = gpdf_data[*cnt - 1].beginRow - gpdf_data[*cnt - 1].sizeRow;	//�I���ʒu�E�s
			gpdf_data[*cnt - 1].endCol = gpdf_data[*cnt - 1].beginCol + gpdf_data[*cnt - 1].sizeCol;	//�I���ʒu�E��
		}
	}

	//�J��Ԃ�(�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�֌J��Ԃ����̗v�f���쐬����)
	curidx = *cnt - 1;	//���݂̌r���v�f(�J��Ԃ��̂Ƃ��̊�v�f)�̓Y������ۑ�
	if (yrep > 0){
		//�c�����J��Ԃ��̍s�Ԋu���擾
		//�\�`���̔���
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(�\�`��)
			yival = getNum(loverlay_data->itd[pidx], 17, 3);	//�c�����J��Ԃ��s�Ԋu(�傫���E�s�Ɏw�肳��Ă���)
		}else{
			//(�\�`���łȂ�)
			yival = getNum(loverlay_data->itd[pidx], 25, 2);	//�c�����J��Ԃ��s�Ԋu
			if (gpdf_data[*cnt - 1].lineStyl == 2){
				//�����`�c�����J��Ԃ��Ȃ�A���s�̂P�s�������Z
				yival += 1;
			}
		}
		if (gpdf_data[*cnt - 1].lineStyl == 1 || gpdf_data[*cnt - 1].lineStyl == 3 || gpdf_data[*cnt - 1].lineStyl == 5){
			//�c���A�����`�܂��͎ΐ��Ȃ玩�g�̒��������Z
			yival += gpdf_data[*cnt - 1].sizeRow;
		}
	}
	if (xrep > 0){
		//�������J��Ԃ��̌��Ԋu���擾
		//�\�`���̔���
		chkF = chkHyo(gpdf_data[*cnt - 1].lineStyl, sizeRow, sizeCol, yrep, xrep);
		if (chkF == 1){
			//(�\�`��)
			xival = getNum(loverlay_data->itd[pidx], 20, 3);	//�������J��Ԃ����Ԋu(�傫���E���Ɏw�肳��Ă���)
		}else{
			//(�\�`���łȂ�)
			xival = getNum(loverlay_data->itd[pidx], 29, 2);	//�������J��Ԃ����Ԋu
			if (gpdf_data[*cnt - 1].lineStyl == 1){
				//�c���`�������J��Ԃ��Ȃ�A�����̂P���������Z
				xival += 1;
			}
		}
		if (gpdf_data[*cnt - 1].lineStyl == 2 || gpdf_data[*cnt - 1].lineStyl == 3 || gpdf_data[*cnt - 1].lineStyl == 5){
			//�����A�����`�܂��͎ΐ��Ȃ玩�g�̒��������Z
			xival += gpdf_data[*cnt - 1].sizeCol;
		}
		if (yrep <= 0){
			//�������J��Ԃ�����ŁA�c�����J��Ԃ��Ȃ��̂Ƃ��A�R�[�h�に��for���[�v���X���[�����̂ŁA
			//�c�����J��Ԃ��� 1 ��ݒ肵�Ă���(�P��J��Ԃ��Ȃ̂ŉe���͂Ȃ�)
			yrep = 1;
		}
	}
	for (yidx = 0; yidx < yrep; yidx++){
		//�c�����J��Ԃ�
		if (yidx > 0){
			//���݂̌r���v�f(�J��Ԃ��̂Ƃ��̊�v�f)������̂łP�ڂ͗v�f���쐬���Ȃ�
			ret = addpdf_dataCnt(cnt);
			if (ret != 0){
				return ret;
			}
			clrpdf_data(*cnt - 1);	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���N���A
			gpdf_data[*cnt - 1].no = *cnt;		//�A��
			gpdf_data[*cnt - 1].levelNo = gpdf_data[curidx].levelNo;	//���x��(�ԍ�)
			gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//�J�n�ʒu�E�s
			gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol;	//�J�n�ʒu�E��
			gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//�傫���E�s
			gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//�傫���E��
			gpdf_data[*cnt - 1].lineStyl = gpdf_data[curidx].lineStyl;	//���`
			gpdf_data[*cnt - 1].lineVar = gpdf_data[curidx].lineVar;	//����
			gpdf_data[*cnt - 1].endRow = gpdf_data[curidx].endRow + (yival * yidx);	//�I���ʒu�E�s
			gpdf_data[*cnt - 1].endCol = gpdf_data[curidx].endCol;	//�I���ʒu�E��
		}
		for (xidx = 0; xidx < xrep; xidx++){
			//�������J��Ԃ�
			if (xidx > 0){
				//���݂̌r���v�f(�J��Ԃ��̂Ƃ��̊�v�f)������̂łP�ڂ͗v�f���쐬���Ȃ�
				ret = addpdf_dataCnt(cnt);
				if (ret != 0){
					return ret;
				}
				clrpdf_data(*cnt - 1);	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���N���A
				gpdf_data[*cnt - 1].no = *cnt;		//�A��
			gpdf_data[*cnt - 1].levelNo = gpdf_data[curidx].levelNo;	//���x��(�ԍ�)
				gpdf_data[*cnt - 1].beginRow = gpdf_data[curidx].beginRow + (yival * yidx);	//�J�n�ʒu�E�s
				gpdf_data[*cnt - 1].beginCol = gpdf_data[curidx].beginCol + (xival * xidx);	//�J�n�ʒu�E��
				gpdf_data[*cnt - 1].sizeRow = gpdf_data[curidx].sizeRow;	//�傫���E�s
				gpdf_data[*cnt - 1].sizeCol = gpdf_data[curidx].sizeCol;	//�傫���E��
				gpdf_data[*cnt - 1].lineStyl = gpdf_data[curidx].lineStyl;	//���`
				gpdf_data[*cnt - 1].lineVar = gpdf_data[curidx].lineVar;	//����
				gpdf_data[*cnt - 1].endRow = gpdf_data[curidx].endRow + (yival * yidx);	//�I���ʒu�E�s
				gpdf_data[*cnt - 1].endCol = gpdf_data[curidx].endCol + (xival * xidx);	//�I���ʒu�E��
			}
		}
	}

	return ret;
}

//�y�����I�[�o���C�t�@�C���̓��e(�\����)�̌��o���l�v�f���J�n�ʒu�E�s�A���ŕ��בւ��z
//��OVERLAY_DATA *loverlay_data�F�����I�[�o���C�t�@�C���̓��e(�\����)(I)
void PR_sortoverlaydata(OVERLAY_DATA *loverlay_data){
	int keya = 0;	//��r�p�L�[�P
	int keyb = 0;	//��r�p�L�[�Q
	int Col = 0;
	int Row = 0;
	int loopF = 0;
	char s_itd[100][423];	//IT��`���בւ��p�̈�
	char wchr[423];
	int s_cnt = 0;
	int idx = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func, '\0', MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_sortoverlaydata :");
	gfuncid = 14;
	gtermid = 0;

#ifdef DEBUG
printf(">> PR_sortoverlaydata  loverlay_data->itd_cnt : %d\n", loverlay_data->itd_cnt);
#endif

	//IT��`���בւ��p�̈揉����
	for (idx = 0; idx < GOVERLAY_DATA_MAX; idx++){
		memset(s_itd[idx], '\0', sizeof(s_itd[idx]));
	}
#ifdef DEBUG
printf(">> PR_sortoverlaydata  sizeof s_itd    : %d\n", sizeof(s_itd));
printf(">> PR_sortoverlaydata  sizeof s_itd[0] : %d\n", sizeof(s_itd[0]));
#endif

	//�����I�[�o���C�t�@�C���̓��e(�\����)IT��`���̌��o���l�̗v�f�݂̂𕶎���z��ɕۑ�
	s_cnt = 0;
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
		if (strcmp(getStr(loverlay_data->itd[idx], 36, 1), "A") == 0 ||
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "N") == 0 ||
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "K") == 0 ||
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "X") == 0){
			s_cnt += 1;
			strcpy(s_itd[s_cnt - 1], loverlay_data->itd[idx]);
		}
	}

	//���o���l�v�f�̕�����z����J�n�ʒu�E�s�A�����L�[�ɕ��בւ�
	loopF = 1;
	while (loopF == 1){
		loopF = 0;
		for (idx = 0; idx < (s_cnt - 1); idx++){
			Row = getNum(s_itd[idx], 11, 3);
			sortoverlaydataRowSet(s_itd[idx], &Row);	//��r�L�[�l�ɂ̓|�C���g�E�g��w��ɂ��s�ύX�𔽉f���Ĕ�r
			Col = getNum(s_itd[idx], 14, 3);
			keya = (Row * 1000) + Col;
			Row = getNum(s_itd[idx + 1], 11, 3);
			sortoverlaydataRowSet(s_itd[idx + 1], &Row);	//��r�L�[�l�ɂ̓|�C���g�E�g��w��ɂ��s�ύX�𔽉f���Ĕ�r
			Col = getNum(s_itd[idx + 1], 14, 3);
			keyb = (Row * 1000) + Col;
			if (keya > keyb){
				strcpy(wchr, s_itd[idx]);
				memset(s_itd[idx], '\0', sizeof(s_itd[idx]));
				strcpy(s_itd[idx], s_itd[idx + 1]);
				memset(s_itd[idx + 1], '\0', sizeof(s_itd[idx + 1]));
				strcpy(s_itd[idx + 1], wchr);
				loopF = 1;
			}
		}
	}

	//�����I�[�o���C�t�@�C���̓��e(�\����)IT��`���̌��o���l�ȊO(=�r��)�̗v�f�݂̂����o���l�v�f�̕�����z��̌�ɕۑ�
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
		if (strcmp(getStr(loverlay_data->itd[idx], 36, 1), "A") != 0 &&
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "N") != 0 &&
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "K") != 0 &&
		    strcmp(getStr(loverlay_data->itd[idx], 36, 1), "X") != 0){
			s_cnt += 1;
			strcpy(s_itd[s_cnt - 1], loverlay_data->itd[idx]);
		}
	}

#ifdef DEBUG
for (idx = 0; idx < s_cnt; idx++){
printf(">> PR_sortoverlaydata  s_cnt[%03d] : [%s]\n", idx, s_itd[idx]);
}
printf(">> PR_sortoverlaydata                   s_cnt : %d\n", s_cnt);
#endif

	//IT��`��ۑ�����������z��(���o���l�͕��בւ��ς�)�������I�[�o���C�t�@�C���̓��e(�\����)IT��`�֖߂�
	for (idx = 0; idx < loverlay_data->itd_cnt; idx++){
    	memset(loverlay_data->itd[idx], '\0', ITD_LEN_MAX);
		strcpy(loverlay_data->itd[idx], s_itd[idx]);
	}
}

//�y�����I�[�o���Cpdf�t�@�C���̓��e�\���̂Ɍr���f�[�^���쐬(���x��=01�A�����`�̌J��Ԃ��p)�z
//��OVERLAY_DATA *loverlay_data�F�����I�[�o���C�t�@�C���̓��e(�\����)(I)
//�@int pidx                   �F�����I�[�o���C�t�@�C���̓��e(�\����)�J�����g�̓Y����(I)
//�@int *cnt                   �F�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�̌���(I-O)
//�@(�Ԓl)(0=����A�ȊO=���̊֐�����̕Ԓl)
int PR_edtpdfDataLineLoop(OVERLAY_DATA *loverlay_data, int pidx, int *cnt){
	char buf[423];
	char bufa[11];
	char bufb[7];
	char bufc[423];
	int yrepcnt = 0;
	int xrepcnt = 0;
	int yrepival = 0;
	int xrepival = 0;
	int ybasStr = 0;
	int xbasStr = 0;
	int ybasSize = 0;
	int xbasSize = 0;
	int ystr = 0;
	int xstr = 0;
	int yloop = 0;
	int xloop = 0;
	int loop = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtpdfDataLineLoop :");
	gfuncid = 15;
	gtermid = 0;

	//�J��Ԃ��E�c�A���E�񐔁A�Ԋu���擾
	yrepcnt = getNum(loverlay_data->itd[pidx], 23, 2);	//�J��Ԃ��E�c�E��
	yrepival = getNum(loverlay_data->itd[pidx], 25, 2);	//�J��Ԃ��E�c�E�Ԋu
	xrepcnt = getNum(loverlay_data->itd[pidx], 27, 2);	//�J��Ԃ��E���E��
	xrepival = getNum(loverlay_data->itd[pidx], 29, 2);	//�J��Ԃ��E���E�Ԋu
	if (yrepcnt == 0){
		yrepcnt = 1;	//�J��Ԃ��E�c�E�񐔂Ȃ��͂P�ɂ���
	}
	if (xrepcnt == 0){
		xrepcnt = 1;	//�J��Ԃ��E���E�񐔂Ȃ��͂P�ɂ���
	}

	ybasStr = getNum(loverlay_data->itd[pidx], 11, 3);	//�J�n�ʒu�E�s
	xbasStr = getNum(loverlay_data->itd[pidx], 14, 3);	//�J�n�ʒu�E��
	ybasSize = getNum(loverlay_data->itd[pidx], 17, 3);	//�傫���E�s
	xbasSize = getNum(loverlay_data->itd[pidx], 20, 3);	//�傫���E��
	for (yloop = 0; yloop < yrepcnt; yloop++){	//�J��Ԃ��E�c�E�񐔕����[�v
		//�J�n�ʒu�E�s�̈ʒu���J��Ԃ����l�����ĎZ�o
		ystr = ybasStr + ((ybasSize + yrepival) * yloop);
		for (xloop = 0; xloop < xrepcnt; xloop++){	//�J��Ԃ��E���E�񐔕����[�v
			//�J�n�ʒu�E���̈ʒu���J��Ԃ����l�����ĎZ�o
			xstr = xbasStr + ((xbasSize + xrepival) * xloop);

			//���x��=01�Ő��`=�����`�̌J��Ԃ��w�肪���鏑���I�[�o���C�t�@�C���̃��R�[�h�̊J�n�ʒu�E�s�A���̒l��u������
			memset(buf, '\0', sizeof(buf));
			memset(bufa, '\0', sizeof(bufa));
			memset(bufb, '\0', sizeof(bufb));
			memset(bufc, '\0', sizeof(bufc));
			sprintf(bufa, "%s", getStr(loverlay_data->itd[pidx], 1, 10));	//(�擪)�`�p��
			sprintf(bufb, "%s", getStr(loverlay_data->itd[pidx], 17, 6));	//�傫���E�s�A��
			sprintf(bufc, "%s", getStr(loverlay_data->itd[pidx], 31, ITD_LEN_MAX));	//(���g�p31�o�C�g��)�`�ŏI���܂�
			//�J��Ԃ��̒l�͍폜(=�󔒂ɒu����)
			sprintf(buf, "%s%3d%3d%s        %s", bufa, ystr, xstr, bufb, bufc);
			strcpy(loverlay_data->itd[pidx], buf);

			//���x��=01�̎w��Ōr���f�[�^���쐬
			ret = PR_edtpdfDataLine(loverlay_data, pidx, cnt);
			if (ret != 0){
				return ret;
			}

			//���̏�ʃ��x�����o������܂�(=�z���̎w��S��)���[�v���Čr���f�[�^���쐬
			for (loop = pidx + 1; pidx < loverlay_data->itd_cnt; loop++){
				if (getNum(loverlay_data->itd[loop], 8, 2) <= 01){
					//��ʃ��x���܂��̓��x���w��Ȃ����݂������烋�[�vExit
					break;
				}
				//���ʃ��x���̎w��Ōr���f�[�^���쐬
				ret = PR_edtpdfDataLine(loverlay_data, loop, cnt);
				if (ret != 0){
					return ret;
				}
			}
		}
	}

	return ret;
}

//�y����������(�����I�[�o���CID)����t�@�C�����AID�����p������Apdf����ҏW�z
//��char *parastr�F����������(�����I�[�o���CID)(I)
//�@(�Ԓl)(0=����)
int PR_edtFileID(char *parastr){
	char overlaypath[] = "../print/";	//�����I�[�o���C�t�@�C���̃p�X
	char txtChar[] = ".txt";			//txt�g���q
	char pdfChar[] = ".pdf";			//pdf�g���q
	char overlayid[128] = "";	//�����I�[�o���CID
	char *str_top;	//�n�C�t����؂�̍ŏ��̕�����
	char *str_bot;	//�n�C�t����؂�̍Ō�̕�����
	char *str_wk;
	char *strpara;
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"PR_edtFileID :");
	gfuncid = 16;
	gtermid = 0;

	str_top = (char *)malloc(sizeof(parastr));
	str_bot = (char *)malloc(sizeof(parastr));
	str_wk = (char *)malloc(sizeof(parastr));
	strpara = (char *)malloc(sizeof(parastr));
	memset(str_top, '\0', sizeof(parastr));
	memset(str_bot, '\0', sizeof(parastr));
	memset(str_wk, '\0', sizeof(parastr));
	memset(strpara, '\0', sizeof(parastr));
	
	//����������(�����I�[�o���CID)�̑啶������������
	strcpy(strpara, parastr);
	//�����񒆂Ƀn�C�t�������邩�H
	if (strchr(strpara, '-')){
		//�ŏ��̃n�C�t���O�̕������ϐ��Ɋi�[
		str_top = strtok(strpara, "-");
		//�ȍ~�ɂ���Ō�̍ŕ���̕������ϐ��Ɋi�[
		str_bot = strtok(NULL, "-");
		strcpy(str_wk, str_bot);
		while (str_wk != NULL){
			str_wk = strtok(NULL, "-");
			if (str_wk != NULL){
				strcpy(str_bot, str_wk);
			}
		}
		//�����I�[�o���C�����p�������ҏW
		sprintf(PR_overlaysearch, "%s%%%s", str_top, str_bot);
	}else{
		//�ŏ��̕������999�A�Ō�̕����������������(�����I�[�o���CID)�Ƃ���
		strcpy(str_top, "999");
		strcpy(str_bot, parastr);
		//�����I�[�o���C�����p�������ҏW
		sprintf(PR_overlaysearch, "%s-%s", str_top, str_bot);
	}
	//
#ifdef DEBUG
printf(">> PR_edtFileID  str_top : %s\n" , str_top);
printf(">> PR_edtFileID  str_bot : %s\n" , str_bot);
#endif
	//�����I�[�o���C�t�@�C��(�p�X+�t�@�C��)��ҏW
	sprintf(PR_overlayfile, "%s%s%s", overlaypath, strToLower(str_bot), txtChar);
	//�����I�[�o���Cpdf�t�@�C��ID
	sprintf(PR_overlaypdf, "%s%s", parastr, pdfChar);
#ifdef DEBUG
printf(">> PR_edtFileID  PR_overlayfile   : %s\n" , PR_overlayfile);
printf(">> PR_edtFileID  PR_overlaysearch : %s\n" , PR_overlaysearch);
printf(">> PR_edtFileID  PR_overlaypdf    : %s\n" , PR_overlaypdf);
#endif

	return ret;
}

//�y�t�H���g���A�p���w��z
int fontSetting(){
	HPDF_REAL lineheight, charwidth, pageheight, pagewidth;
	HPDF_Font definition_font;          //pdf�̃t�H���g
	HPDF_STATUS retCode;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," fontSetting :");
	gfuncid = 51;
	gtermid = 0;

	//�t�H���g�ݒ�
	definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);
	retCode = HPDF_Page_SetFontAndSize (PR_page, definition_font, PR_fontsize);
	
	//�������݊J�n
//	retCode = HPDF_Page_BeginText(PR_page);
	retCode = HPDF_Page_SetFontAndSize (PR_page, definition_font, PR_fontsize);
	
	//��s�̍����̐ݒ�
	pageheight = HPDF_Page_GetHeight(PR_page);
	//�㉺�̃}�[�W�������A�����̂���1.8�{�Ƃ���
	lineheight = ((pageheight - (PR_topmargin * (2 * MARGIN_CONDITION))) / PR_linepitch );
#ifdef DEBUG
printf(">> fontSetting PR_linepitch/lineheight : %d / %f\n" , PR_linepitch, lineheight);
#endif
	//���p������/2?
	retCode = HPDF_Page_SetTextLeading(PR_page,(lineheight / 2));

	//���p�P�������̉����|�C���g���擾
	PR_charsize = HPDF_Page_TextWidth(PR_page, " ");
	//�s�����擾
	PR_lineheight = HPDF_Page_GetTextLeading(PR_page);
#ifdef DEBUG
printf(">> fontSetting PR_charsize/PR_lineheight : %f / %f\n" , PR_charsize, PR_lineheight);
#endif
	//�t�H���g�ݒ���擾
	PR_definition_font = HPDF_GetFont(pdf, PR_font, PR_DEFAULT_ENCODE);

	return retCode;
}

//�y�ݒ�t�@�C���̃p�X���擾�z
//�J�����gdir��conf�������J���Ƃǂ��łł����s�ł��Ȃ��̂�
char *getConfFilename(char *strConfPath){
	char strTime[] = "00/00/00 00:00:00";
	FILE *fpFileExist;      //20150828 add koyama 

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getConfFilename :");
	gfuncid = 52;
	gtermid = 0;

	if ((fpFileExist = fopen(CONF_FLIEPATH, "r")) == NULL){
		if ((fpFileExist = fopen(CONF_SHREPATH, "r")) == NULL){
			fprintf(stderr, "Error C [%02d]:conf read Error %s \n", local_server_time(strTime));
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

//�y�t�H���g�t�@�C���̃p�X�{�t�@�C�������擾�z
char *getFontFilePath(char *fontname,char *filePath){
	char strConfPath[1024]; //20150828 add koyama �t�@�C�������󂯎�邽�߂̃|�C���^

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getFontFilePath :font %s filepath %.15s ",fontname,filePath);
	gfuncid = 53;
	gtermid = 0;

	int i;
	//�t�@�C���l�[�������Ƀ��[�_�|�C���^���쐬
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//���[�_�����[�h�ł����Ԃ�
	xmlTextReaderRead(reader);
	//���݂̃m�[�h�̃|�C���^���Z�b�g�H
	xmlTextReaderExpand(reader);
	//���݂̃m�[�h����DOM�����o���Ă���?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc) return NULL;
	//�h�L�������g����R���e�L�X�g()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
	if (!ctx) return NULL;
	//xpath�Ŏw�肵���m�[�h���X�g���擾
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CHAR_SET_DEFPATH, ctx);
	//xpath�Ŏw�肵���m�[�h���X�g���擾
	if (!xpobj) return NULL;
	//�m�[�h���X�g���m�[�h�̔z��̂悤�Ȃ��̂�
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//�m�[�h���̎擾(�擾�ł��Ȃ��Ȃ�0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//�m�[�h���X�g����l��\��
	for (i = 0; i < size; ++i) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, i);
			if (node->children->next->children->content) {
				//�ݒ�t�@�C������DB�̃z�X�g�����擾
				if (i == 0){
					//�t�@�C���p�X�̑}��(�e�L�X�g�m�[�h�������)
					strcpy(filePath,node->children->next->next->next->children->content);
				}
				if (strcmp(node->children->next->children->content,fontname) == 0){
					//�t�@�C���p�X�̑}��(�e�L�X�g�m�[�h�������)
					strcpy(filePath,node->children->next->next->next->children->content);
				}
			} else {
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				return NULL;
			}
		}
	}
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	return filePath;
}

//�y�t�@�C���̗L�����m�F�z
//��char *chkFile�F�t�@�C��ID(�p�X�{�t�@�C����)(I)
//�@(�Ԓl)�t�@�C���̗L���i0=�t�@�C������A1=�t�@�C���Ȃ�)
int chkFileExist(char *chkFile){
	int ret = 0;
	struct stat st;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," chkFileExist :format %s ",chkFile);
	gfuncid = 54;
	gtermid = 0;

	//�t�@�C���̏����擾���邱�Ƃɂ��t�@�C���̗L�����m�F
	ret = stat(chkFile, &st);
	
	return ret;
}

//�y���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss)�ŕԂ��z
char *local_server_time(char *retStr){
	time_t timer;
	struct tm *date;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," local_server_time :");
	gfuncid = 56;
	gtermid = 0;

	timer = time(NULL);
	date = localtime(&timer);
	strftime(retStr,strlen("00/00/00 00:00:00"),"%y/%m/%d %H:%M:%S", date);
	return retStr;
}

//�y���p���S�p�ϊ��z
//��char *han�F���p������(I)
//�@char *zen�F�S�p������(O)
void cnvHanToZen(const char *han, char *zen)
{
	unsigned char c1;	//(�S�p�P�ʂ�)1�o�C�g��
	unsigned char c2;	//(�S�p�P�ʂ�)2�o�C�g��
	int idx = 0;
	int pos = 0;
	int i = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," cnvHanToZen :");
	gfuncid = 57;
	gtermid = 0;

	idx = 0;
	pos = 0;
	//�����̕�������P�o�C�g���Ƃɕϊ�
	while (han[idx] != '\0'){
		c1 = han[idx];		//�P�o�C�g��
		c2 = han[idx + 1];	//���̂P�o�C�g
#ifdef DEBUG
printf(">>          cnvHanToZen c1 / c2 : %c / %c (hex) %02x / %02x\n", c1, c2, c1, c2);
#endif
		if (ISKANJI(c1)){
			//�S�p�����Ɣ���
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kanji\n");
#endif
			zen[pos] = c1;
			zen[pos + 1] = c2;
			pos += 2;
			idx += 2;
		}else if (ISALPH(c1)){
			//���p�p�����Ɣ���
			i = c1 - 0x20;
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is alphabet : [%s](%d)\n", ALPH[i], i);
printf(">>        >>cnvHanToZen ALPH_1(%d) : %02x\n", i, ALPH[i][0]);
printf(">>        >>cnvHanToZen ALPH_2(%d) : %02x\n", i, ALPH[i][1]);
#endif
			zen[pos] = ALPH[i][0];
			zen[pos + 1] = ALPH[i][1];
			pos += 2;
			idx += 1;
		}else if (ISKANA(c1)){
			i = c1 - 0xa1;
			if (MAYBEDAKU(c1) && ISDAKU(c2)){
				//���p�J�i���_�����Ɣ���
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kana dakuten\n");
#endif
				zen[pos] = DAKU[i][0];
				zen[pos + 1] = DAKU[i][1];
				pos += 2;
				idx += 2;
			}else if (MAYBEHANDAKU(c1) && ISHANDAKU(c2)){
				//���p�J�i�����_�����Ɣ���
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kana handakuten\n");
#endif
				zen[pos] = HDAK[i][0];
				zen[pos + 1] = HDAK[i][1];
				pos += 2;
				idx += 2;
			}else{
				//���p�J�i�����Ɣ���
#ifdef DEBUG
printf(">>        >>cnvHanToZen c1 is kana nomal\n");
#endif
				if (i == 30){
					//�S�p�u�\(0x835c)�v�̓R�[�h�ɂ��Œ�l�w��ł��Ȃ��̂Œ��ڕҏW
					zen[pos] = 0x83;
					zen[pos + 1] = 0x5c;
					pos += 2;
					idx += 1;
				}else{
					zen[pos] = KANA[i][0];
					zen[pos + 1] = KANA[i][1];
					pos += 2;
					idx += 1;
				}
			}
		}
	}

	zen[pos] = '\0';
}

//�y�����_(�|�C���g)�A���ʒu�A������(�|�C���g)���猅�ʒu�̃|�C���g�l�����߂�z
//��HPDF_Point basPos  �F��_�|�C���g(I)
//�@int Col            �F���ʒu(I)
//�@HPDF_REAL charWidth�F���p�P�������̕�(�|�C���g)(I)
//�@(�Ԓl)���̈ʒu(�|�C���g)
HPDF_REAL getColpos(HPDF_Point basPos, int Col, HPDF_REAL charWidth){
	HPDF_REAL fColpos = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getColpos :");
	gfuncid = 58;
	gtermid = 0;

	//�������F1/2�������O��
	fColpos = basPos.x + (float)Col * charWidth - (charWidth / 2);
	//�������F1/3�������O��
	fColpos -= charWidth / 3;

	return fColpos;
}

//�y�����_(�|�C���g)�A�s�ʒu�A������(�|�C���g)����s�ʒu�̃|�C���g�l�����߂�z
//��HPDF_Point basPos  �F��_�|�C���g(I)
//�@int Row            �F�s�ʒu(I)
//�@HPDF_REAL charWidth�F���p�P�������̕�(�|�C���g)(I)
//�@int drawline       �F�r���i0=���o���l�A1=�r���j
//�@int centerline     �F�����`�s�����i0=�s�����łȂ��A1=�s�����j
//�@(�Ԓl)�s�̈ʒu(�|�C���g)
HPDF_REAL getRowpos(HPDF_Point basPos, int Row, HPDF_REAL lineHeight, int drawline, int centerline){
	HPDF_REAL fRowpos = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," getRowpos :");
	gfuncid = 59;
	gtermid = 0;

	//�������F1/2�s�����
	fRowpos = basPos.y - (float)Row * (lineHeight * 2) + lineHeight;

	//�r��(�������F1/3�s������)
	if (drawline == 1){
		fRowpos -= lineHeight / 3;
	}

	//�s�����`����(�ʒu�������w��Ȃ�1/2�s���)
	if (centerline == 1){
		fRowpos += lineHeight;
	}

	return fRowpos;
}

//�y������̎w��ʒu����w�蒷���������o���z
//��char *sbuf�F�؂�o����������(I)
//�@int pos   �F�؂�o���J�n�ʒu(I)
//�@int getlen�F�؂�o����(I)
//(�Ԓl)�؂�o����������
char *getStr(char *sbuf, int pos, int getlen){
	int maxlen = 0;
	int len = 0;
	int idx = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getStr :");
	gfuncid = 60;
	gtermid = 0;

#ifdef DEBUG
//printf(">> getStr - sbuf/pos/getlen : %s / %d / %d\n" , sbuf, pos, getlen);
#endif
	//�؂�o����������̒����擾
	maxlen = strlen(sbuf);
#ifdef DEBUG
//printf("getStr - maxlen : %d\n" , maxlen);
#endif

	len = 0;
	memset(gcutbuf, '\0', sizeof(gcutbuf));
	//�؂�o���ʒu����؂�o�������A�������؂�o��
	if (pos <= maxlen){
		//�؂�o����������̒��ȓ��̂Ƃ��̂ݐ؂�o�����s��
		for (idx = 0; idx < 100; idx++){
			//�w�茅���ɒB������I��
			if (len >= getlen){
				break;
			}
			//�؂�o����������̒��ɒB������I��
			if (idx > (maxlen - 1)){
				gcutbuf[idx] = '\0';
				break;
			}
			//�؂�o���������񂩂�؂�o���敶����ւP�������ҏW(NULL����������I������)
			gcutbuf[idx] = sbuf[pos - 1];
			if (gcutbuf[idx] == '\0'){
				break;
			}
			pos += 1;
			len += 1;
		}
	}
#ifdef DEBUG
//printf(">> getStr - idx/pos/len : %d / %d / %d\n" , idx, pos, len);
//printf(">> getStr - gcutbuf : %s\n" , gcutbuf);
#endif

	return gcutbuf;
}

//�y������̎w��ʒu����w�蒷���������o���Đ��l��(int�^�̂�)�z
//��char *sbuf�F�؂�o����������(I)
//�@int pos   �F�؂�o���J�n�ʒu(I)
//�@int getlen�F�؂�o����(I)
//(�Ԓl)���l�����ꂽ�؂�o��������
int getNum(char *sbuf, int pos, int len){
	char buf[20];
	int maxlen = 0;
	int idx = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getNum :");
	gfuncid = 61;
	gtermid = 0;

	//�؂�o����������̒����擾
	maxlen = strlen(sbuf);
	//�؂�o�����������������Ƃ��̓[����Ԃ�
	if (pos > maxlen){
		return ret;
	}
	if ((pos + len - 1) > maxlen){
		return ret;
	}

	memset(buf, '\0', 20);
	strncpy(buf, sbuf + (pos - 1), len);
#ifdef DEBUG
//printf(">> getNum     buf(bef.) : %s\n" , buf);
#endif
	for (idx = 0; idx < 20 || buf[idx] != '\0'; idx++){
		if (buf[idx] == ' '){
			buf[idx] = '0';
		}
	}
#ifdef DEBUG
//printf(">> getNum     buf(aft.) : %s\n" , buf);
#endif
	ret = atoi(buf);

	return ret;
}

//�y������̕������𐔂���(�S�p�������P����)�z
//��char *sbuf�F�J�E���g�Ώە�����(I)
//�@(�Ԓl)������̕�����
int getCharCnt(char *sbuf){
	unsigned char ch;
	int cnt = 0;
	int idx = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"getCharCnt :");
	gfuncid = 62;
	gtermid = 0;

	cnt = 0;
	for (idx = 0; idx < strlen(sbuf) && sbuf[idx] != '\0'; idx++){
		ch = sbuf[idx];
		//�S�p����
		//(�P�o�C�g��)
		if ((ch >= 0x81 && ch <= 0x9f) || (ch >= 0xe0 && ch <= 0xfc)){
			//(�Q�o�C�g��)
			ch = sbuf[idx + 1];
			if (ch >= 0x40 && ch <= 0xfc){
				idx += 1;	//�S�p�����Ȃ̂łP�o�C�g���Z���Ď�������
			}
		}
		cnt += 1;
	}
	ret = cnt;

	return ret;
}

//�y�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�������Z�z
//��int *cnt�F�����I�[�o���Cpdf�t�@�C���̓��e����(I-O)
//�@(�Ԓl)(0=�v�f���Z�����A-1=�\���̍ő吔�I�[�o�[)
int addpdf_dataCnt(int *cnt){
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"addpdf_dataCnt :");
	gfuncid = 63;
	gtermid = 0;

	*cnt += 1;
#ifdef DEBUG
//printf(">> addpdf_dataCnt     cnt : %d\n", *cnt);
#endif
	if (*cnt > GPDF_DATA_MAX){
		gtermid = 1;
		fprintf(stderr," Error C [%02d-%02d]: pdf File structure table overflow\n", gfuncid, gtermid);
		return 1;
	}
	
	return ret;
}

//�y�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���������z
void clrpdf_data(int idx){

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," clrpdf_data :");
	gfuncid = 64;
	gtermid = 0;

	gpdf_data[idx].no = 0;			//�A��
	gpdf_data[idx].levelNo = 0;		//���x��(�ԍ�)
	gpdf_data[idx].beginRow = 0;	//�J�n�ʒu�E�s
	gpdf_data[idx].beginCol = 0;	//�J�n�ʒu�E��
	gpdf_data[idx].sizeRow = 0;		//�傫���E�s
	gpdf_data[idx].sizeCol = 0;		//�傫���E��
	gpdf_data[idx].lineStyl = 0;	//���`�@(1:�c��(=V)�A2:����(=H)�A3:�����`(=B)�A5:�ΐ�(=O))
	gpdf_data[idx].lineVar = 0;		//����@(1:�����A2:�א��A3:���_���A4:�ד_���A9:����_�����A10:�׈�_����(=A)�A11:����(=O)�A12:���_��(=P)�A13:����_����(=S))
	gpdf_data[idx].linePos = 0;		//�ʒu�@(0:����(=D)�A1:����(=C))
	gpdf_data[idx].endRow = 0;		//�I���ʒu�E�s
	gpdf_data[idx].endCol = 0;		//�I���ʒu�E��
	gpdf_data[idx].point = 0;		//�|�C���g
	gpdf_data[idx].zoom = 0;		//�g��
	gpdf_data[idx].ranhaba = 0;		//����
	gpdf_data[idx].interval = 0;	//����s�P��Ԃ̃o�C�g��(�擪�̒P��̓[����ݒ�)
	gpdf_data[idx].spaceCharLen = 0;	//�����ԋ󔒁i�P��=������(������)�F���o���l�A�����`�t���p�j
	memset(gpdf_data[idx].text, '\0', 349);	//���o���l
}

//�yIT��`�E�r���̃��x�����e(�\���̎���)���������z
void clrlevel_data(){
	int idx = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," clrlevel_data :");
	gfuncid = 65;
	gtermid = 0;

	for (idx = 0; idx < level_data_len; idx++){
		glevel_data[idx].levelNo = 0;	//���x��(�ԍ�)
		glevel_data[idx].lineStyl = 0;	//���`
		glevel_data[idx].beginRow = 0;	//�J�n�ʒu�E�s
		glevel_data[idx].beginCol = 0;	//�J�n�ʒu�E��
		glevel_data[idx].sizeRow = 0;	//�傫���E�s
		glevel_data[idx].sizeCol = 0;	//�傫���E��
	}
}

//�y���o���l�̕ҏW�i�c�����E�������j�z
//��OVERLAY_DATA *loverlay_data�F�����I�[�o���C�t�@�C���̓��e(�\����)(I)
//�@int pidx                   �F�����I�[�o���C�t�@�C���̓��e(�\����)�J�����g�̓Y����(I)
//�@int *cnt                   �F�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�̌���(I-O)
//�@char *buf                  �F���o���l�̓��e(I)
//�@int *retRow                �F���o���l���o�͂������ݍs(I-O)
//�@(�Ԓl)(0=����)
int edtpdfText(OVERLAY_DATA *loverlay_data, int pidx, int *cnt, char *buf, int *retRow){
	unsigned char ch;
	int fstF = 0;
	int charLen = 0;
	int curCnt = 0;
	int edtRow = 0;
	int idx = 0;
	int j = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," edtpdfText :");
	gfuncid = 66;
	gtermid = 0;

	//���o���l(�c��������)
	if (strcmp(getStr(loverlay_data->itd[pidx], 48, 1), "Y") == 0){
		//�c����
		if (strlen(buf) > 0){
			curCnt = *cnt - 1;	//1�����ڂ̔z��v�f�Y�����l��ۑ�
			edtRow = gpdf_data[curCnt].beginRow;	//1�����ڂ̊J�n�ʒu�E�s��ϐ��ɐݒ�
			fstF = 1;
			for (idx = 0; idx < strlen(buf); idx++){
				if (buf[idx] == '\0'){
					break;
				}
				if (fstF != 1){
					//(�������Z)
					ret = addpdf_dataCnt(cnt);
					if (ret != 0){
						return ret;
					}
					clrpdf_data(*cnt - 1);	//�����I�[�o���Cpdf�t�@�C���̓��e(�\���̎���)�w��Y�����v�f���N���A
					//(���o���l�ȊO�͓���̒l���i�[)
					gpdf_data[*cnt - 1].no = *cnt;		//�A��
					edtRow += 1;
					gpdf_data[*cnt - 1].beginRow = edtRow;						//�J�n�ʒu�E�s
					gpdf_data[*cnt - 1].beginCol = gpdf_data[curCnt].beginCol;	//�J�n�ʒu�E��
					gpdf_data[*cnt - 1].point = gpdf_data[curCnt].point;		//�|�C���g
					gpdf_data[*cnt - 1].zoom = gpdf_data[curCnt].zoom;			//�g��
					gpdf_data[*cnt - 1].ranhaba = gpdf_data[curCnt].ranhaba;	//����
				}
				charLen = 1;
				//�S�p����
				//(�P�o�C�g��)
				ch = buf[idx];
				if ((ch >= 0x81 && ch <= 0x9f) || (ch >= 0xe0 && ch <= 0xfc)){
					//(�Q�o�C�g��)
					ch = buf[idx + 1];
					if (ch >= 0x40 && ch <= 0xfc){
						charLen = 2;
					}
				}
				//�����o�C�g���؂�o���ĕҏW
				for (j = 0; j < charLen; j++){
					gpdf_data[*cnt - 1].text[j] = buf[idx + j];	//���o���l
				}
				gpdf_data[*cnt - 1].text[j] = '\0';
				idx = idx + charLen - 1;	//�S�p�����Ȃ�P�o�C�g���Z���Ď�������

				fstF = 0;
			}
			*retRow = gpdf_data[*cnt - 1].beginRow;
		}
	}else{
		//������
		strcpy(gpdf_data[*cnt - 1].text, buf);	//���o���l
		*retRow = gpdf_data[*cnt - 1].beginRow;
	}

	return ret;
}

//�y�|�C���g�E�g��ɂ��f�t�H���g����������������ꍇ�ɂP�s���Z�z
//��char *s_itd�FIT��`�̂P�v�f(I)
//�@int *Row   �F�g��w��ɂ��␳���ꂽ�s�ʒu(I-O)
void sortoverlaydataRowSet(char *s_itd, int *Row){

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"sortoverlaydataRowSet :");
	gfuncid = 67;
	gtermid = 0;

	//Y,Z�F�|�C���g100���ȏ�
	//F�F����
	if (strcmp(getStr(s_itd, 39, 1), "Y") == 0 ||
		strcmp(getStr(s_itd, 39, 1), "Z") == 0){
		if (strcmp(getStr(s_itd, 40, 1), "F") != 0){
			*Row += 1;
		}
	}else if (strcmp(getStr(s_itd, 40, 1), "T") == 0){
		*Row += 1;
	}
}

//�y�����I�[�o���Cpdf�t�@�C���̓��e�\���̂̍s�A���𒲐�����z
//��int *cnt�F�����I�[�o���Cpdf�t�@�C���̓��e(�\����)�v�f��
//�@(�Ԓl)(0=����)
int setpdfDataColRow(int *cnt){
	int idx = 0;
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"setpdfDataColRow :");
	gfuncid = 68;
	gtermid = 0;

	for (idx = 0; idx < *cnt; idx++){
		//�s�F���f�[�^���e����Ƃ̊֌W�łP�s���ݒ肷��
		gpdf_data[idx].beginRow -= 1;	//�J�n�ʒu�E�s
		gpdf_data[idx].endRow -= 1;	//�I���ʒu�E�s
		//�c���F���̉E���Ɉ����d�l�Ȃ̂Ō��ɂP���Z����
		if (gpdf_data[idx].lineStyl == 1){
			gpdf_data[idx].beginCol += 1;	//�J�n�ʒu�E��
			gpdf_data[idx].endCol += 1;	//�I���ʒu�E��
		}
	}

	return ret;
}

//�y���x���w��z����IT�w�肪�\�`�����`�F�b�N�z
//��int lineStyl�F���`
//�@int sizeRow �F�傫���E�s
//�@int sizeCol �F�傫���E��
//�@int yrep    �F�J��Ԃ��E�c�E��
//�@int xrep    �F�J��Ԃ��E���E��
//�@(�Ԓl)(0=�\�`���łȂ��A1=�\�`��)
int chkHyo(int lineStyl, int sizeRow, int sizeCol, int yrep, int xrep){
	int ret = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"chkHyo :");
	gfuncid = 69;
	gtermid = 0;

	ret = 0;
	if (lineStyl == 1){
		//�c��
		if (sizeRow == 0 && sizeCol != 0 && yrep == 0){
			//�c���w��ő傫���E�s�Ɏw�肪�Ȃ��A�傫���E���Ɏw�肪����A�J��Ԃ��E�c�񐔂Ɏw�肪�Ȃ����\�`���Ɣ���
			ret = 1;
		}
	}else if (lineStyl == 2){
		//����
		if (sizeRow != 0 && sizeCol == 0 && xrep == 0){
			//�����w��ő傫���E�s�Ɏw�肪�����āA�傫���E���Ɏw�肪�Ȃ��A�J��Ԃ��E���񐔂Ɏw�肪�Ȃ����\�`���Ɣ���
			ret = 1;
		}
	}

	return ret;
}

//�y������E���̋󔒕������폜�z
//��char *str�F�����Ώە�����(I-O)
void RTrim(char *str){
	int len = 0;
	int idx = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"RTrim :");
	gfuncid = 70;
	gtermid = 0;

	//�����񒷂��擾
	len = strlen(str);

	//�����񖖔�����󔒈ȊO�̕����������܂ŋ󔒕�����NULL�����ɒu������
	for (idx = len - 1; idx > 0; idx--){
		if (str[idx] != '\0' && str[idx] != ' '){
			break;
		}
		str[idx] = '\0';
	}
}

//�y������̑啶�����������ɕϊ��z
//��char *str�F�ϊ���������(I)
//�@(�Ԓl)(�����񒆂̑啶�����������ɕϊ����ꂽ������)
char *strToLower(char *str){
	int pos = 0;
	char *strS = NULL,*strE = NULL;
	
	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"strToLower :");
	gfuncid = 71;
	gtermid = 0;
	strS = str;
	strE = str + strlen(str);
	for (; strS < strE; strS++){
		//�啶���̂Ƃ����u���ŏ������ɒu������
		*strS = tolower(*strS);
	}
#ifdef DEBUG
printf(">> toLower  str   : %s\n" , str);
printf(">> toLower  gstrup : %s\n" , gstrup);
#endif
	
	return str;
}





/*
char *toLower(char *str){
	int pos = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"toLower :");
	gfuncid = 71;
	gtermid = 0;

	for (pos = 0; pos < strlen(str); pos++){
		if (str[pos] >= 'A' && str[pos] <= 'Z'){
			//�啶���̂Ƃ����u���ŏ������ɒu������
			str[pos] += 0x20;
		}
	}
#ifdef DEBUG
printf(">> toLower  str   : %s\n" , str);
#endif
	
	return str;
}
*/
