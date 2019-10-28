// Nikodem Sytniejewski 11ID-SP
#include <iostream>
#include <stdio.h>
#include <cstdlib>
#include <math.h>
#include <conio.h>
#include <time.h>
using namespace std;
int main()
{
	char znak;
	int wybor;
	do
	{
		int opcja, menu;
		cout<<"Nikodem Sytniejewski, drugi program."<<endl<<endl;
		cout<<"Wybierz jedna z opcji:"<<endl;
		cout<<"1 - zagraj w totolotka :-) program wylosuje 6 liczb z 49-ciu"<<endl;
		cout<<"2 - rozwiaz rownanie kwadratowe ax^2+bx+c=0, wpisujac dowolne niewiadome a,b,c"<<endl;
		cout<<"3 - maksymalna i minimalna wartosc w dowolnie wylosowanym zbiorze"<<endl<<endl;
		cout<<"Twoj wybor: ";
		cin>>wybor;
        switch(wybor)
        {
        	case 1:
        		{
        			int i;
        			srand(time(NULL));
        			for(int i=1;i<=6;i++)
					{	
    					cout<<rand()%49+1<<endl;	
					}
					cout<<"UWAGA! Gdyby liczby sie powtarzaly, nalezy powtorzyc losowanie!"<<endl<<endl;
        		}
        		break;
        		case 2:
        			{
        				
						{
						  	float a;
    						float b;
    						float c;
   							float delta;
    						cout<<"a=";
    						cin>>a;
    						cout<<"b=";
    						cin>>b;
    						cout<<"c=";
    						cin>>c;
    						if(a==0)
    						{
     							if(b==0)
     							{
      								if(c==0)
      								{
       									cout<<"Nieskoñczenie wiele rozwiazan"<<endl;      
      								} 
      								else
      								{
       									cout<<"Rozwiazanie :"<<-c/b<<endl;
      								}                
     							}               
    						}  
    						else
    						{	
     							delta=b*b-4*a*c;
     							if(delta<0)
      							{
       								cout<<"Nie ma rozwiazan"<<endl;
      							}
    							else
     							{
      								if(delta==0) 
      								{
      									cout<<"Rozwiazanie: "<<-b/(2*a)<<endl;
      								}
      								else
      								{
      									if(delta>0)
      									{
       										cout<<"Rozwiazanie 1: "<<(-b-sqrt(delta))/2*a<<endl;
       										cout<<"Rozwiazanie 2: "<<(-b+sqrt(delta))/2*a<<endl;
       									}
      								}                    
     							}      
    						}						}
        			}
        			break;
        			case 3:
        				{
        					int a,i,s,p,n,min,max;
							cout<<"Podaj liczbe losowan (n) oraz zbior od 0 do p, z ktorego maja byc losowane liczby."<<endl;
							cout<<"Podaj n: ";
							cin>>n;
							cout<<"Podaj p: ";
							cin>>p;
							srand(time(NULL));
							s=0;
							max=-5;
							min=100000000;
							for(i=1;i<=n;i++)
							{
								a=rand()%p;
								cout<<"Losowanie nr: "<<i<<": "<<a<<endl;
								s=s+a;
								if(a>max)max=a;
								if(a<min)min=a;
							}
							cout<<"Min= "<<min<<endl;
							cout<<"Max= "<<max<<endl;
							cout<<"Liczba losowan to: "<<i-1<<endl;
        				}
    	}
		cout<<"Czy chcesz zakonczyc prace mojego programu? Jesli tak wpisz 1, jesli nie kliknij dowolny klawisz."<<endl;
    	cout<<"Twoj wybor: ";
    	cin>>znak;	
	}
	while (znak!='1');
    return 0;
}
