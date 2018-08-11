#include <cstdio>
#include<stdlib.h>
#include<ctype.h>
#include<stdio.h>
#include<string>
#include<iostream>
#include<fstream>
#include <iomanip>    //列对其
using namespace std;
#define    OK                  0
#define    ERROR              -1
#define int8       1
#define int10      2
#define int16      3
#define IF         4
#define THEN       5
#define ELSE       6
#define WHILE      7
#define DO         8
#define ident      9 // 字符
#define add        10// +
#define minus      11// -
#define multi      12// *
#define div        13// /
#define gt         14// >
#define lt         15// <
#define eq         16// =
#define lp         17// (
#define rp         18// )
#define semic      19// ;
#define start      20
#define sum         5
#define MAX        500
void grammar_analysis_s(struct S_Attr *as);
void grammar_analysis_c(struct C_Attr *gac);
void grammar_analysis_e(struct E_Attr *gae);
void grammar_analysis_t(struct T_Attr *gat);
void grammar_analysis_f(struct F_Attr *gaf);

/***********定义全局变量*************/
char    ch;
char   *keyword[sum]={"if","then","else","while","do"};

typedef  struct
{
	int word_id;   //种别
	char buf[30];
}token;

ifstream  infile;                              //输入文件类对象

/***********************************词法分析***************************************/

token scan()
{

	token token_w;
	for(int j=0;j<=9;j++)
		token_w.buf[j]='\0';
	infile.get(ch);
	if(infile.eof()) ch=-1;
	while(ch == 32 || ch == '\n' || ch == '\t')
	{
		infile.get(ch);
		if(infile.eof()) ch=-1;
	}
	token_w.buf[0]=ch;
	int i=1;
	if(ch > '0' && ch <= '9')//十进制
	{
		while(isdigit(ch))
		{

			infile.get(ch);
			if(infile.eof()) ch=-1;
			if(!isdigit(ch))
			{
				infile.seekg(-1,ios::cur);
				token_w.word_id=int10;
				return(token_w);
			}
			token_w.buf[i]=ch;
			i++;
		}
	}
	//**************************************************************************8,10,16
	else if(ch == '0')                              //ch 为0
	{  
		infile.get(ch);
		if(ch>= '0' && ch <= '7')               //八进制
		{
			token_w.buf[i]=ch;
			infile.get(ch);
			if(infile.eof()) ch=-1;
			while(ch>= '0' && ch <= '7')             
			{   
				i++;   
				token_w.buf[i]=ch;
				infile.get(ch);	
				if(infile.eof()) ch=-1;
			}
			infile.seekg(-1,ios::cur);
			token_w.word_id=int8;
			return(token_w);
		}
		else if(ch==';'||ch==' ') //十进制
		{
			infile.seekg(-1,ios::cur);
			token_w.word_id=int10;
			return(token_w);
		}
		else if(ch=='8' || ch=='9')
		{
			token_w.buf[i]=ch;
			token_w.word_id=ERROR;
			return(token_w);
		}
		else if( ch == 'x' || ch == 'X') //16进制
		{
			token_w.buf[i]=ch;
			infile.get(ch);
			if(infile.eof()) ch=-1;
			while(isdigit(ch) || ch >= 'a' && ch <= 'f')
			{
				i++;
				token_w.buf[i]=ch;
				infile.get(ch);
				if(infile.eof()) ch=-1;
			}	
			if(0==strcmp(token_w.buf,"0x") || 0==strcmp(token_w.buf,"0X"))
			{
				token_w.word_id=ERROR;	
				return(token_w);
			}
			infile.seekg(-1,ios::cur);
			token_w.word_id=int16;
			return(token_w);	
		}
	}
	///********************************************标识符、关键字
	else if(isalpha(ch))   //标识符
	{
		infile.get(ch);
		if(infile.eof()) ch=-1;
		while(isalnum(ch)||ch==95)
		{
			token_w.buf[i]=ch;
			i++;
			infile.get(ch);
			if(infile.eof()) ch=-1;
			if(!isalnum(ch) && ch != 95) break;
		}
		infile.seekg(-1,ios::cur);
		token_w.word_id=ident;
		for(int j=0;j<sum;j++)
		{
			if(0==strcmp(keyword[j],token_w.buf))
			{
				token_w.word_id=j+4;
				return(token_w);
			}
		}
		return(token_w);
	}
	//*********************************************************************运算符
	else {
		switch(ch)
		{
		case'+':
			token_w.word_id=add;
			break;             //break  

		case'-':token_w.word_id=minus;break;
		case'*':token_w.word_id=multi;break;
		case'/':token_w.word_id=div;break;
		case';':token_w.word_id=semic;break;
		case'>':token_w.word_id=gt;break;
		case'<':token_w.word_id=lt;break;
		case'(':token_w.word_id=lp;break;
		case'=':token_w.word_id=eq;break;
		case')':token_w.word_id=rp;break;
		default:token_w.word_id=-1;return(token_w);
		}
	}

	infile.get(ch);
	if(infile.eof()) ch=-1;
	infile.seekg(-1,ios::cur);

	return(token_w);
}

/***********************************语法分析***************************************************************/
token  lookhead;
struct S_Attr{
	char pCode[MAX];     //s.code
	int  ibegin;
	int  inext;
};
struct C_Attr{
	char pCode[MAX];
	int  iflase;
	int  itrue;
};
struct E_Attr{
	char  pCode[MAX];
	char  place[MAX];
};
struct T_Attr{
	char  pCode[MAX];
	char  place[MAX];
};
struct F_Attr{
	char  pCode[MAX];
	char  place[MAX];
};
struct id_Attr{
	char  name[MAX];
};
struct int_Attr{
	char  value[MAX];
};
int newlabel()       //地址   
{
	static int i=-1;
	i++;
	return i;
}
char * newplace()    //变量
{
	char  temp[10];
	static int i=1;
	sprintf(temp,"%s%d","t",i);
	i++;
	return temp;
}
int match(int tk)
{
	if(  lookhead.word_id == tk)
		lookhead=scan();
	else
		cout<<"此处接受字符不匹配！"<<endl;
	return OK;
}
int judge_s()
{
	static int jt=0;
	if (lookhead.word_id == ident)
	{
		jt=0;
	}
	else if (lookhead.word_id==IF)
	{
		jt=1;
	}
	else if(lookhead.word_id==WHILE)
	{
		if( jt==1 )
		{
			return OK;     //   返回0
		}
		else jt=1;
	}
	return ERROR;  //返回1
}
void grammar_analysis_s( struct S_Attr * as )
{                                   
	if(  lookhead.word_id == ident )              //判断是不是标识符
	{
		judge_s();
		struct id_Attr ide;
		sprintf(ide.name,"%s",lookhead.buf);      //固有属性 
		struct E_Attr  ae;
		match(ident);
		match(eq);    
		grammar_analysis_e(&ae);                  //调用E函数  
		sprintf(as->pCode,"%s%s%s%s\n",ae.pCode,ide.name,"=:",ae.place);

	}
	else if( lookhead.word_id == IF )
	{
		judge_s();
		struct C_Attr ac;
		struct S_Attr as1;
		ac.itrue=newlabel();
		ac.iflase=as1.inext=as->inext;
		match(IF);
		grammar_analysis_c(&ac);                  //调用c函数
		match(THEN);

		grammar_analysis_s(&as1);                 //调用S函数
		sprintf(as->pCode,"%sL%d:\n%s",ac.pCode,ac.itrue,as1.pCode);                //L%d:\n,ac.iflase
	}

	else if( lookhead.word_id == WHILE )
	{
		struct C_Attr ac;
		struct S_Attr as1;
		int rt=judge_s();
		if (rt==0)
		{
			ac.itrue=newlabel();
			as->ibegin=ac.itrue-1;
		}
		else{
			as->ibegin=newlabel();
			ac.itrue=newlabel();
		}
		ac.iflase=as->inext;
		as1.inext=as->ibegin;
		match(WHILE);
		grammar_analysis_c(&ac);                  //调用c函数
		match(DO);
		grammar_analysis_s(&as1);                 //调用S函数
		if (rt==0)
		{
			sprintf(as->pCode,"%sL%d:%s%sL%d\n",ac.pCode,ac.itrue,as1.pCode,"goto",as->ibegin);
		}
		else
			sprintf(as->pCode,"L%d:%sL%d:%s%sL%d\n",as->ibegin,ac.pCode,ac.itrue,as1.pCode,"goto",as->ibegin);
	}
	else exit(ERROR);                        
}

void grammar_analysis_c(struct C_Attr *gac)
{
	struct E_Attr   age;
	struct E_Attr   age2;
	grammar_analysis_e(&age);                      //调用e函数
	if( lookhead.word_id == gt )                    //>
	{
		match(gt);
		grammar_analysis_e(&age2);                  //调用e函数
		sprintf(gac->pCode,"%s%s%s%s%s%s%sL%d\n%sL%d\n",age.pCode,age2.pCode,"if  ",age.place," > ",age2.place,"  goto  ",gac->itrue,"    goto  ",gac->iflase);
	}
	else if(lookhead.word_id==eq )                   //=
	{
		match(eq);
		grammar_analysis_e(&age2);              //调用e函数
		sprintf(gac->pCode,"%s%s%s%s%s%s%sL%d\n%sL%d\n",age.pCode,age2.pCode,"if  ",age.place," = ",age2.place,"  goto  ",gac->itrue,"    goto  ",gac->iflase);
	}
	else if( lookhead.word_id==lt )           // <
	{
		match(lt);
		grammar_analysis_e(&age2);              //调用e函数
		sprintf(gac->pCode,"%s%s%s%s%s%s%sL%d\n%sL%d\n",age.pCode,age2.pCode,"if  ",age.place," < ",age2.place,"  goto  ",gac->itrue,"    goto  ",gac->iflase);
	}
	else
		exit(ERROR);		
}
void grammar_analysis_e(struct E_Attr *gae)
{
	char temp_m[MAX];
	char temp_p[MAX];
	temp_p[0]='\0';
	//sprintf(gae->place,"%s",newplace());
	struct T_Attr  agt;
	struct T_Attr  agt2; 
	grammar_analysis_t(&agt);                          //调用t函数
	sprintf(gae->place,"%s",agt.place);
	sprintf(gae->pCode,"%s",agt.pCode);
	while( lookhead.word_id == minus || lookhead.word_id == add ) 
	{
		if( lookhead.word_id == minus )                //  -
		{
			match(minus);
			grammar_analysis_t(&agt2);                 //调用t函数
			sprintf( temp_m,"%s",newplace());
			sprintf(temp_p,"   %s%s%s%s%s\n",temp_m,"=:",gae->place,"-",agt2.place);   //gen
			sprintf(gae->pCode,"%s%s%s",gae->pCode,agt2.pCode,temp_p);
			sprintf( gae->place,"%s",temp_m);

		}
		else if( lookhead.word_id == add )             //  +
		{
			match(add);
			grammar_analysis_t(&agt2);                 //调用t函数
			sprintf( temp_m,"%s",newplace());
			sprintf(temp_p,"%s%s%s%s%s\n",temp_m,"=:",gae->place,"+",agt2.place);
			sprintf(gae->pCode,"%s%s%s",gae->pCode,agt2.pCode,temp_p);
			sprintf( gae->place,"%s",temp_m);
		}
	}
}
void grammar_analysis_t(struct T_Attr *gat)
{

	char temp_m[MAX];
	char temp_p[MAX];
	temp_p[0]='\0';
	//sprintf(gat->place,"%s",newplace());
	struct F_Attr  agf;
	struct F_Attr  agf2;
	grammar_analysis_f(&agf);                     //调用f函数
	sprintf(gat->place,"%s",agf.place);
	sprintf(gat->pCode,"%s",agf.pCode);
	while(  lookhead.word_id == div || lookhead.word_id == multi )
	{	
		if( lookhead.word_id == div )             // / 
		{
			match(div);
			grammar_analysis_f(&agf2);                 //调用f函数
			sprintf( temp_m,"%s",newplace());
			sprintf(temp_p,"%s%s%s%s%s\n",temp_m,"=:",gat->place,"/",agf2.place);   //gen
			sprintf(gat->pCode,"%s%s%s",gat->pCode,agf2.pCode,temp_p);
			sprintf( gat->place,"%s",temp_m);
		}
		else if( lookhead.word_id == multi )     // *
		{ 
			match(multi);
			grammar_analysis_f(&agf2);           //调用f函数
			sprintf( temp_m,"%s",newplace());
			sprintf(temp_p,"%s%s%s%s%s\n",temp_m,"=:",gat->place,"*",agf2.place);   //gen
			sprintf(gat->pCode,"%s%s%s",gat->pCode,agf2.pCode,temp_p);
			sprintf( gat->place,"%s",temp_m);
		}
	}
}
void grammar_analysis_f(struct F_Attr *gaf)
{
	int temp_a=0;
	if( lookhead.word_id == lp )                 // (
	{
		struct E_Attr fse;
		match(lp);
		grammar_analysis_e(&fse);                //调用e函数
		sprintf(gaf->place,"%s",fse.place);
		sprintf(gaf->pCode,"%s",fse.pCode);
		if( lookhead.word_id == rp )             // )
			match(rp);
		else exit(ERROR);
	}
	else if( lookhead.word_id == ident )
	{
		struct id_Attr   sid;
		sprintf(sid.name,"%s",lookhead.buf);
		sprintf(gaf->place,"%s",sid.name);
		gaf->pCode[0]='\0';
		match(ident);
	}
	else if( lookhead.word_id == int8 )
	{
		struct  int_Attr  sit8;
		sprintf(sit8.value,"%s",lookhead.buf);
		sprintf(gaf->place,"%s",sit8.value);
		gaf->pCode[0]='\0';
		match(int8);
	}
	else if( lookhead.word_id == int10 )
	{
		struct  int_Attr  sit10;
		sprintf(sit10.value,"%s",lookhead.buf);
		sprintf(gaf->place,"%s",sit10.value);
		gaf->pCode[0]='\0';
		match(int10);
	}
	else if( lookhead.word_id == int16 )
	{
		struct  int_Attr  sit16;
		sprintf(sit16.value,"%s",lookhead.buf);
		sprintf(gaf->place,"%s",sit16.value);
		gaf->pCode[0]='\0';
		match(int16);
	}
	else   exit(ERROR);
}

int main()
{
	int at;
	infile.open("myfile2.txt");
	lookhead=scan();        //预读一个
	struct S_Attr Sp;
	Sp.inext=newlabel();
	grammar_analysis_s(&Sp);
	sprintf(Sp.pCode,"%sL%d:\n",Sp.pCode,Sp.inext);

	cout<<Sp.pCode<<endl;
	cin>>at;
	return 0;
}