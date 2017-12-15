/* header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libcob.h>
#include "confheader.h"

#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN]="";
int MT_Initialize();
#endif


#define min(a,b) ((a) < (b) ? (a) : (b))
#define MAX_STRINGS 20		//���̓t�@�C���Ǎ��ő啶����

/* �������cobol�ɓ]�ʂ���֐� */
void C3_move_to_cob(char *cob_dat, const char *dat)
{
    int len = strlen(cob_dat);                      // data length in cob
    memset(cob_dat, ' ', min(len, strlen(dat)));    // clear with spaces
    memcpy(cob_dat, dat, min(len, strlen(dat)));    // data copy
    return;
}

//�������ɓn���ꂽ�ϐ���,JRCODE��path�����ĕԂ�
//Return�͊��ϐ������ꂽ0,path�͎�ꂽ�̂ō����1,path���Ȃ�����3
int getJrcodeFilePath(char *iId,char *filePathPointer){
  char *tmpFilePathPointer=NULL;
  int returnStatus=0;
  tmpFilePathPointer = getenv("MAP_JRCODE_FILE");
  if(tmpFilePathPointer == NULL){
  	//�t�@�C�����uFILE_NAME + iId�v�쐬
  	tmpFilePathPointer = getenv( "JRCODE_PATH" );
    returnStatus = 1;
  }
  //��ꂽ�|�C���^��
	if(tmpFilePathPointer != NULL){
		strcat(filePathPointer, tmpFilePathPointer);
	}else{
		strcat(filePathPointer, "");
    returnStatus = 3;
	}
	strcat(filePathPointer,iId);
  fprintf(stderr," Error C [%02d]:  [%s] : \n",99,filePathPointer);
  return returnStatus;
}

/* JRCODE�Ǎ��� */
/* �����F
		iId		:���O�C��ID,
		oValue	:JRCODE�̒l
*/
int C3_Get_Jrcode(char *iId,char *oValue){
	int ret = 0;
	int cnt = 0;	//���[�v�J�E���^
	char *chrDefPath;
	char fName[100];
	char data[MAX_STRINGS];		//�Ǎ�������
	FILE *fp;			//�t�@�C���|�C���^�[

	//�����񏉊���
	fName[0] ='\0';

  getJrcodeFilePath(iId,fName);

	if((fp = fopen(fName,"w")) != NULL ){
		// �t�@�C�������݂���ꍇ

		/* �t�@�C���f�[�^���� */
		while(fgets(data, MAX_STRINGS, fp) != NULL){
			if(cnt == 0){
				//1�����ڂ̂݃f�[�^�擾
				cnt += 1;
				C3_move_to_cob(oValue, data);
			}else{
				//2�����ڈڍs�����݂���΃G���[
				ret = 1;
				break;
			}
		}

		/* �t�@�C���E�N���[�Y */
		fclose(fp);
	}else{
		// �t�@�C�������݂��Ȃ��ꍇ(fopen�Ńt�@�C�����쐬�ł��Ȃ������ꍇ)
		ret = 1;
	}

	return ret;
}

/* JRCODE������ */
/* �����F
		iId		:���O�C��ID,
		iValue	:JRCODE�̒l
*/
int C3_Set_Jrcode(char *iId,char *code,int *iValue){
	int ret = 0;
	char *chrDefPath;
	char fName[100]="";
	char chrCode[4]="";
	FILE *fp;

	//�����񏉊���
	fName[0] ='\0';

	if(cob_call_params != 3){
		fprintf(stderr," Error C [%02d]: C3_Set_Jrcode is not match Argument count \n",99);
		exit(1);
	}

	//�����𕶎��ɕϊ�
	sprintf(chrCode,"%03d",*iValue);

  getJrcodeFilePath(iId,fName);

	/* �t�@�C���E�I�[�v�� */
	if((fp = fopen(fName, "w")) != NULL){

		/* �t�@�C���f�[�^���� */
		if(fputs(chrCode, fp) == EOF){
			ret = 1;
		}

		//�t�@�C���ւ̏������݂��ł����猳��CODE�ւ��Ԃ�
		memcpy(code,chrCode,strlen(chrCode));

		/* �t�@�C���E�N���[�Y */
		fclose(fp);

	}else{
		// �t�@�C�������݂��Ȃ��ꍇ
		ret = 1;
	}

	return ret;
}
