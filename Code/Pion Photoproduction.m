(* ::Package:: *)

(* ::Input:: *)
(*Define Data Import Functions*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)
(*(*Define the Data Import Functions*)*)
(*importdata[link_,length_]:=Module[{rawdata,data},rawdata=Import[link,"Table"];data=Select[Select[rawdata,Length[#]==length&],VectorQ[#,NumberQ]&];data];*)
(*importenergy[link_,pattern_]:=ToExpression[StringCases[StringCases[Import[link,"Text"],pattern],DigitCharacter..~~"."~~DigitCharacter..]];*)
(**)


(* ::Input:: *)
(*Data Import*)


(* ::Input:: *)
(*dataDXS=importdata["https://dl.dropboxusercontent.com/u/61536361/dsigma_dOmega.txt",4];*)
(*dataASY=importdata["https://dl.dropboxusercontent.com/u/61536361/Sigma_asymmetry.txt",4];*)
(*dataT=importdata["https://dl.dropboxusercontent.com/u/61536361/sigmaT_SvenPeter.txt",4];*)


(* ::Text:: *)
(*Creates arrays of the data (Scattering-Angle, Observable, Error) from the raw data*)


(* ::Input:: *)
(*diffDXS=Table[{((Pi/180)*dataDXS[[i,2]]),dataDXS[[i,3]],dataDXS[[i,4]]},{i,1,Length[dataDXS]}];*)
(*diffT=Table[{((Pi/180)*dataT[[i,1]]),dataT[[i,2]],dataT[[i,3]]},{i,1,Length[dataT]}];*)


(* ::Text:: *)
(*Patitions the data into the individual energy bands*)


(* ::Input:: *)
(*angleDXS=Partition[diffDXS,20];*)
(*angleT=Partition[diffT,18];*)
(**)
(*asymmetry=Table[{dataASY[[i,1]],ArcCos[dataASY[[i,2]]],dataASY[[i,3]],dataASY[[i,4]]},{i,1,Length[dataASY]}];*)
(*asypart=Table[{},{i,1,23}];*)
(*For[i=1;j=1,i<Length[asymmetry],If[asymmetry[[i+1,1]]<asymmetry[[i,1]],j++];i++,AppendTo[asypart[[j]],Drop[asymmetry[[i]],1]]];*)
(*AppendTo[asypart[[j]],Drop[asymmetry[[i]],1]];*)
(*Clear[i,j]*)
(*angleASY=asypart;*)


(* ::Text:: *)
(*Imports the energy data into mathematica *)


(* ::Input:: *)
(*tmpDXS=importenergy["https://dl.dropboxusercontent.com/u/61536361/dsigma_dOmega.txt","W="~~Shortest[___]~~"\n"];*)
(*tempASY=importenergy["https://dl.dropboxusercontent.com/u/61536361/Sigma_asymmetry.txt","W="~~Shortest[___]~~"+/-"];*)
(*tmpT=importenergy["https://dl.dropboxusercontent.com/u/61536361/sigmaT_SvenPeter.txt","E ="~~Shortest[___]~~"\n"];*)


(* ::Text:: *)
(*Converts the energy in lab frame to center of mass frame and creates a table*)


(* ::Input:: *)
(*energyDXS=Table[Mean[tmpDXS[[i]]],{i,1,Length[tmpDXS]}];*)
(*energyASY=Flatten[tempASY];*)
(*energyT=Table[(2*tmpT[[i,1]]*938.28+938.28^2)^.5,{i,1,Length[tmpT]}];*)


(* ::Text:: *)
(*Functional Definitions of Multipoles*)


(* ::Input:: *)
(*T0={{1,0,0,0,0,0,0,0},{0,6,0,0,0,0,0,0},{0,0,18,0,0,0,0,0},{0,0,0,2,0,0,0,0},{0,0,0,0,2,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,9,0},{0,0,0,0,0,0,0,6}};*)
(*T1={{0,3,0,0,1,-1,0,0},{3,0,72/5,-3/5,0,0,9/5,-9/5},{0,72/5,0,0,0,0,0,0},{0,-3/5,0,0,1,-1,0,0},{1,0,0,1,0,0,27/5,3/5},{-1,0,0,-1,0,0,0,3},{0,9/5,0,0,27/5,0,0,0},{0,-9/5,0,0,3/5,3,0,0}};*)
(*T2={{0,0,6,1,0,0,3,-3},{0,3,0,0,3,-3,0,0},{6,0,108/7,-12/7,0,0,36/7,-36/7},{1,0,-12/7,-1,0,0,3,-3},{0,3,0,0,-1,-1,0,0},{0,-3,0,0,-1,0,0,0},{3,0,36/7,3,0,0,36/7,9/7},{-3,0,-36/7,-3,0,0,9/7,3}};*)
(*T3={{0,0,0,0,0,0,0,0},{0,0,18/5,18/5,0,0,36/5,-36/5},{0,18/5,0,0,6,-6,0,0},{0,18/5,0,0,0,0,0,0},{0,0,6,0,0,0,-12/5,-18/5},{0,0,-6,0,0,0,-3,0},{0,36/5,0,0,-12/5,-3,0,0},{0,-36/5,0,0,-18/5,0,0,0}};*)
(*T4={{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,18/7,54/7,0,0,90/7,-90/7},{0,0,54/7,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,90/7,0,0,0,-36/7,-72/7},{0,0,-90/7,0,0,0,-72/7,0}};*)
(*S0={{0,0,3/2,3/2,0,0,-3/2,3/2},{0,-9/2,0,0,3/2,-3/2,0,0},{3/2,0,-24,6,0,0,6,-6},{3/2,0,6,-3/2,0,0,-3/2,3/2},{0,3/2,0,0,3/2,3/2,0,0},{0,-3/2,0,0,3/2,0,0,0},{-3/2,0,6,3/2,0,0,12,21/2},{3/2,0,-6,3/2,0,0,21/2,9/2}};*)
(*S1={{0,0,0,0,0,0,0,0},{0,0,-27/2,9,0,0,0,0},{0,-27/2,0,0,15/2,-15/2,0,0},{0,9,0,0,0,0,0,0},{0,0,15/2,0,0,0,6,9},{0,0,-15/2,0,0,0,15/2,0},{0,0,0,0,6,15/2,0,0},{0,0,0,0,9,0,0,0}};*)
(*S2={{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,-30,45/2,0,0,15/2,-15/2},{0,0,45/2,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,15/2,0,0,0,15,30},{0,0,-15/2,0,0,0,30,0}};*)
(*F0={{0,-3/2,0,0,3/2,0,0,0},{-3/2,0,-15/2,0,0,0,15/2,0},{0,-15/2,0,0,-5/2,1,0,0},{0,0,0,0,0,-3/2,0,0},{3/2,0,-5/2,0,0,0,5/2,0},{0,0,1,-3/2,0,0,-1,-3/2},{0,15/2,0,0,5/2,-1,0,0},{0,0,0,0,0,-3/2,0,0}};*)
(*F1={{0,0,-6,3/2,0,0,6,3/2},{0,-9,0,0,3,3/2,0,0},{-6,0,-36,-3/2,0,0,9,9/2},{3/2,0,-3/2,3,0,0,3/2,-3},{0,3,0,0,3,-3/2,0,0},{0,3/2,0,0,-3/2,0,0,0},{6,0,9,3/2,0,0,18,-9/2},{3/2,0,9/2,-3,0,0,-9/2,-9}};*)
(*F2={{0,0,0,0,0,0,0,0},{0,0,-39/2,3,0,0,6,9},{0,-39/2,0,0,11/2,5,0,0},{0,3,0,0,3,0,0,0},{0,0,11/2,3,0,0,8,-3},{0,0,5,0,0,0,-5,0},{0,6,0,0,8,-5,0,0},{0,9,0,0,-3,0,0,0}};*)
(*F3={{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,-36,9/2,0,0,9,45/2},{0,0,9/2,0,0,0,9,0},{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0},{0,0,9,9,0,0,18,-9},{0,0,45/2,0,0,0,-9,0}};*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*mpion=138.03;*)
(*mnucl=938.27;*)
(**)
(*ec[w_,theta_]:=*)
(*(F1=1/128. (32. (4. E2minus[W]+9. Em[4,w]+4. E0plus[W]+9. E2plus[W])+450. Ep[4,w]+8. Cos[2. theta] (60. Em[4,w]+60. E2plus[W]+105. Ep[4,w]-48. M2minus[W]-20. Mm[4,w]+48. M2plus[W]+20. Mp[4,w])+70. Cos[4. theta] (9. Ep[4,w]+16. (-Mm[4,w]+Mp[4,w]))+2. Cos[theta] (192. Em[3,w]+360. Em[5,w]+192. E1plus[W]+360. Ep[3,w]+525. Ep[5,w]-64. M1minus[W]-24. Mm[3,w]-15. Mm[5,w]+64. M1plus[W]+24. Mp[3,w]+15. Mp[5,w])+63. Cos[5. theta] (11. Ep[5,w]+25. (-Mm[5,w]+Mp[5,w]))+5. Cos[3. theta] (112. Em[5,w]+112. Ep[3,w]+9. (21. Ep[5,w]-16. Mm[3,w]-7. Mm[5,w]+16. Mp[3,w]+7. Mp[5,w])));*)
(*F2=1/64 (64 M1minus[W]+144 (3+5 Cos[2 theta]) Mm[3,w]+75 (15+28 Cos[2 theta]+21 Cos[4 theta]) Mm[5,w]+64 (2 M1plus[W]+3 (3+5 Cos[2 theta]) Mp[3,w])+16 Cos[theta] (24 M2minus[W]+20 (1+7 Cos[2 theta]) Mm[4,w]+36 M2plus[W]+25 (1+7 Cos[2 theta]) Mp[4,w])+90 (15+28 Cos[2 theta]+21 Cos[4 theta]) Mp[5,w]);*)
(*F3=1/64 (192 Em[3,w]+1200 Em[5,w]+192 E1plus[W]+1200 Ep[3,w]+3675 Ep[5,w]+64 M1minus[W]+624 Mm[3,w]+2325 Mm[5,w]-64 M1plus[W]-624 Mp[3,w]+24 Cos[theta] (40 Em[4,w]+40 E2plus[W]+175 Ep[4,w]+4 (4 M2minus[W]+25 Mm[4,w]-4 M2plus[W]-25 Mp[4,w]))+280 Cos[3 theta] (9 Ep[4,w]+4 Mm[4,w]-4 Mp[4,w])+60 Cos[2 theta] (28 Em[5,w]+28 Ep[3,w]+105 Ep[5,w]+12 Mm[3,w]+63 Mm[5,w]-12 Mp[3,w]-63 Mp[5,w])+315 Cos[4 theta] (11 Ep[5,w]+5 Mm[5,w]-5 Mp[5,w])-2325 Mp[5,w]);*)
(*F4=3/8 (-8 E2minus[W]-50 Em[4,w]-8 E2plus[W]-50 Ep[4,w]-8 M2minus[W]-50 Mm[4,w]+8 M2plus[W]-70 Cos[2 theta] (Em[4,w]+Ep[4,w]+Mm[4,w]-Mp[4,w])+50 Mp[4,w]-5 Cos[theta] (8 Em[3,w]+35 Em[5,w]+8 Ep[3,w]+35 Ep[5,w]+8 Mm[3,w]+35 Mm[5,w]-8 Mp[3,w]-35 Mp[5,w])-105 Cos[3 theta] (Em[5,w]+Ep[5,w]+Mm[5,w]-Mp[5,w]));*)
(*F1=-I*(4.*3.14159*w)/Sqrt[mnucl*mnucl]*F1;*)
(*F2=-I*(4.*3.14159*w)/Sqrt[mnucl*mnucl]*F2;*)
(*F3=-I*(4.*3.14159*w)/Sqrt[mnucl*mnucl]*F3;*)
(*F4=-I*(4.*3.14159*w)/Sqrt[mnucl*mnucl]*F4;*)
(*dsdo=Abs[F1]^2+0.5*(Abs[F2]^2 +Abs[F3]^2+Abs[F4]^2 + 2.*Re[(F1+F3*Cos[theta])*Conjugate[F4]]   )*(Sin[theta])^2;*)
(*easymmetry=(Abs[F1]^2.+Re[ Conjugate[F2]*(F3+F4*Cos[theta]) + Conjugate[F1]*F4 ]*(Sin[theta])^2.)/dsdo;*)
(*fasymmetry=-Re[Conjugate[F2]*(F1+F4*(Sin[theta])^2.)-Conjugate[F1]*(F3+F4*Cos[theta])]*Sin[theta]/dsdo;*)
(*tasymmetry=Im[(-F2+F3+F4*Cos[theta])*Conjugate[F1]+(F3+F4*Cos[theta])*Conjugate[F4]*(Sin[theta])^2.]*Sin[theta]/dsdo;*)
(*sasymmetry=0.5*(Abs[F2]^2.-Abs[F3]^2.-Abs[F4]^2.-2.*Re[(F1+F3*Cos[theta])*Conjugate[F4]])*(Sin[theta])^2./dsdo;*)
(*qeta=1./(2.*w)*Sqrt[(w^2.-(mnucl+m\[Pi]0)^2.)*(w^2.-(mnucl-m\[Pi]0)^2.)];*)
(*qgamma=1./(2.*w)*(w^2.-mnucl^2.);*)
(*dsdotrue=mnucl*mnucl/(4.*3.14159*w)^2.*Abs[qeta]/Abs[qgamma]*dsdo/100.;*)
(*{easymmetry,fasymmetry,tasymmetry,sasymmetry,dsdotrue});*)


(* ::Input:: *)
(*vec={E0p,E1p,E2p,E2m,M1p,M1m,M2p,M2m};*)
(**)
(**)
(*Wt[\[Theta]_,E0p_,E1p_,E2p_,E2m_,M1p_,M1m_,M2p_,M2m_]=Re[Conjugate[vec].T0.vec]+Re[Conjugate[vec].T1.vec]*LegendreP[1,Cos[\[Theta]]]+Re[Conjugate[vec].T2.vec]*LegendreP[2,Cos[\[Theta]]]+Re[Conjugate[vec].T3.vec]*LegendreP[3,Cos[\[Theta]]]+Re[Conjugate[vec].T4.vec]*LegendreP[4,Cos[\[Theta]]];*)
(**)
(*Ws[\[Theta]_,E0p_,E1p_,E2p_,E2m_,M1p_,M1m_,M2p_,M2m_]=(Re[Conjugate[vec].S0.vec]+Re[Conjugate[vec].S1.vec]*LegendreP[1,Cos[\[Theta]]]+Re[Conjugate[vec].S2.vec]*LegendreP[2,Cos[\[Theta]]])*Sin[\[Theta]]^2;*)
(**)
(*Wf[\[Theta]_,E0p_,E1p_,E2p_,E2m_,M1p_,M1m_,M2p_,M2m_]=(Re[Conjugate[vec].F0.vec]+Re[Conjugate[vec].F1.vec]*LegendreP[1,Cos[\[Theta]]]+Re[Conjugate[vec].F2.vec]*LegendreP[2,Cos[\[Theta]]]+Re[Conjugate[vec].F3.vec]*LegendreP[3,Cos[\[Theta]]])*Sin[\[Theta]];*)


(* ::Text:: *)
(*Parameters and constants*)


(* ::Input:: *)
(*m\[Pi]p=139.57; (* mass of \[Pi]^+ *)*)
(*m\[Pi]0=134.98; (* mass of \[Pi]^0 *)*)
(*Mp=938.27; (* mass of proton *)*)
(*Mn=939.565;*)
(*\[Beta]=3.35/(1000*m\[Pi]p); (* cusp parameter *)*)
(*factor=3.894*10^8;  *)
(*(* unit conversion factor: ([MeV]^-2) to [\[Mu]barn] *)*)


(* ::Title:: *)
(*Define the Multipole Functions*)


(* ::Input:: *)
(*nmaxs=4;*)
(*nmaxp=4;*)
(*nmaxd=4;*)
(*nmaxsi=2;*)
(*nmaxpi=0;*)
(*onsi=1;*)
(*onpi=1;*)
(*ond=1;*)
(*q[W_,m_]=1/(2*W) (\[Sqrt]((W^2-(m-Mp)^2)*(W^2-(m+Mp)^2))); (* center of mass frame momentum *)*)
(*qn[W_]=1/(2*W) (\[Sqrt]((W^2-(m\[Pi]p-Mn)^2)*(W^2-(m\[Pi]p+Mn)^2)));*)
(*\[Omega][W_]=Sqrt[q[W,m\[Pi]0]^2+m\[Pi]0^2]; (* pion energy *);*)
(*E0plus[W_]=1/1000 (1/m\[Pi]p)*(Sum[e[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxs}])+onsi*I*\[Beta]*qn[W]/m\[Pi]p*(Sum[ei[i]/10^-(i)*(qn[W]/m\[Pi]p)^(2*i),{i,0,nmaxsi}]);*)
(*P1[W_]=1/1000 (1/m\[Pi]p)*(q[W,m\[Pi]0]/m\[Pi]p*Sum[p1[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxp}]+onpi*I*q[W,m\[Pi]0]^4/m\[Pi]p^4 Sum[p1i[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,0}]);*)
(*P2[W_]=1/1000 (1/m\[Pi]p)*(q[W,m\[Pi]0]/m\[Pi]p*Sum[p2[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxp}]+onpi*I*q[W,m\[Pi]0]^4/m\[Pi]p^4 Sum[p2i[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,0}]);*)
(*P3[W_]=1/1000 (1/m\[Pi]p)*(q[W,m\[Pi]0]/m\[Pi]p*Sum[p3[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxp}]+onpi*I*q[W,m\[Pi]0]^4/m\[Pi]p^4 Sum[p3i[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,0}]);*)
(*E1plus[W_]=1/6*(P1[W]+P2[W]); (* p-wave *)*)
(*M1plus[W_]=1/6*(P1[W]-P2[W])+P3[W]/3; (* p-wave *)*)
(*M1minus[W_]=1/3*(P3[W]+P2[W]-P1[W]); (* p-wave *)*)
(*E2plus[W_]=1/1000 (1/m\[Pi]p)*(q[W,m\[Pi]0]^2/m\[Pi]p^2*(Sum[d2p0[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxd}]))*ond;*)
(*E2minus[W_]=1/1000 (1/m\[Pi]p)*(q[W,m\[Pi]0]^2/m\[Pi]p^2*(Sum[d2m0[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxd}]))*ond;*)
(*M2plus[W_]=1/1000 (1/m\[Pi]p)*(q[W,m\[Pi]0]^2/m\[Pi]p^2*(Sum[dm2p0[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxd}]))*ond;*)
(*M2minus[W_]=1/1000 (1/m\[Pi]p)*(q[W,m\[Pi]0]^2/m\[Pi]p^2*(Sum[dm2m0[i]/10^-(i)*((\[Omega][W]-m\[Pi]0)/m\[Pi]p)^i,{i,0,nmaxd}]))*ond;*)
(**)
(*Dxs[W_,\[Theta]_]:=factor*q[W,m\[Pi]0]/q[W,0]*Wt[\[Theta],E0plus[W],E1plus[W],E2plus[W],E2minus[W],M1plus[W],M1minus[W],M2plus[W],M2minus[W]]*)
(**)
(*\[CapitalSigma][W_,\[Theta]_]:=-Ws[\[Theta],E0plus[W],E1plus[W],E2plus[W],E2minus[W],M1plus[W],M1minus[W],M2plus[W],M2minus[W]]/Wt[\[Theta],E0plus[W],E1plus[W],E2plus[W],E2minus[W],M1plus[W],M1minus[W],M2plus[W],M2minus[W]]*)
(**)
(*A=ec[W,\[Theta]]/.Flatten[{Table[{Ep[i,W]->0,Em[i,W]->0,Mp[i,W]->0,Mm[i,W]->0},{i,3,5}]}];*)
(*sigmaT[W_,\[Theta]_]=A[[3]]*A[[5]]*factor*10^2;*)


(* ::Input:: *)
(*parms=Flatten[Join[Table[{e[i]},{i,0,nmaxs}],Table[{p1[i],p2[i],p3[i]},{i,0,nmaxp}],Table[{d2p0[i],d2m0[i],dm2p0[i],dm2m0[i]},{i,0,nmaxd}],Table[{ei[i]},{i,0,nmaxsi}],Table[{p1i[i],p2i[i],p3i[i]},{i,0,nmaxpi}]]];*)
(*Parms=Flatten[Join[Table[{e[i]},{i,0,nmaxs}],Table[{p1[i],p2[i],p3[i]},{i,0,nmaxp}],Table[{d2p0[i],d2m0[i],dm2p0[i],dm2m0[i]},{i,0,nmaxd}],Table[{ei[i]},{i,0,nmaxsi}],Table[{p1i[i],p2i[i],p3i[i]},{i,0,nmaxpi}]]];*)
(**)


(* ::Subsection:: *)
(*Define \[Chi]^2 With the LASSO*)


(* ::Input:: *)
(*Chisq[\[Lambda]_]:=Sum[(1/AngleDXS[[i,j,3]])^2*(Dxs[EnergyDXS[[i]],AngleDXS[[i,j,1]]]-AngleDXS[[i,j,2]])^2,{i,1,Length[AngleDXS]},{j,1,Length[AngleDXS[[i]]]}]+Sum[(1/angleASY[[i,j,3]])^2*(\[CapitalSigma][energyASY[[i]],angleASY[[i,j,1]]]-angleASY[[i,j,2]])^2,{i,1,Length[angleASY]},{j,1,Length[angleASY[[i]]]}]+Sum[(1/AngleT[[i,j,3]])^2*(sigmaT[EnergyT[[i]],AngleT[[i,j,1]]]-AngleT[[i,j,2]])^2,{i,1,Length[AngleT]},{j,1,Length[AngleT[[i]]]}]+\[Lambda]^4*Sum[Abs[parms[[i]]],{i,1,Length[parms]}];*)


(* ::Subsection:: *)
(*Initialized Guesses and Run through the LASSO Routine*)


(* ::Input:: *)
(*Parms={{e[0],-0.4535575460681614`},{e[1],-0.39824968299305474`},{e[2],-1.2608827224897667`*^-8},{e[3],-6.747275303446579`*^-9},{e[4],-0.00064397827097114994`},{p1[0],9.692069521667984`},{p2[0],-9.464459025227383`},{p3[0],10.370018406489685`},{p1[1],0.4494357050842037`},{p2[1],-2.355139838724338`},{p3[1],0.8416839776362222`},{p1[2],-4.941176916514989`*^-9},{p2[2],-0.002212688277288109`},{p3[2],3.511767442356004`*^-7},{p1[3],-8.820031520098282`*^-7},{p2[3],-1.5694245809466437`*^-8},{p3[3],-7.648263268625836`*^-9},{p1[4],-0.00007739137590943576`},{p2[4],0.0001190951289160686`},{p3[4],-0.00004194067055840057`},{d2p0[0],-0.0007081714535802684`},{d2m0[0],-1.1770155564894006`*^-9},{dm2p0[0],-9.558367398545279`*^-9},{dm2m0[0],0.0008562038079498057`},{d2p0[1],-1.5446695702253405`*^-8},{d2m0[1],-5.395676961882261`*^-9},{dm2p0[1],2.3854278673173978`*^-9},{dm2m0[1],-2.8897424039043888`*^-9},{d2p0[2],-3.975742685623888`*^-9},{d2m0[2],-5.562342985473301`*^-10},{dm2p0[2],0.0002398870891657044`},{dm2m0[2],-1.2051601587072032`*^-8},{d2p0[3],0.001967186241741635`},{d2m0[3],-4.314048753480795`*^-9},{dm2p0[3],-2.9261990299821504`*^-9},{dm2m0[3],-0.0032103149165979505`},{d2p0[4],-0.00025598289826616054`},{d2m0[4],-0.00040079561602610617`},{dm2p0[4],-1.967667695755569`*^-8},{dm2m0[4],-3.596595395325732`*^-9},{ei[0],0.7152191473733729`},{ei[1],0.0009588176915899808`},{ei[2],-0.00023018581831353046`},{p1i[0],-5.435735068824109`*^-9},{p2i[0],-5.068001881583312`*^-9},{p3i[0],-6.84327588484269`*^-9}};*)
(*init=Parms;*)
(**)
(*chilist={};*)
(*chinoplist={};*)
(*palist={};*)
(*nopalist={};*)
(*Do[*)
(*fitresults=FindMinimum[Chisq[\[Lambda]],Parms,MaxIterations->Infinity,Gradient->"FiniteDifference"];*)
(*chisqwp=fitresults[[1]];*)
(*pars=fitresults[[2]];*)
(*chisq=Chisq[0]/.pars;*)
(*AppendTo[chilist,{\[Lambda],chisqwp}];*)
(*AppendTo[chinoplist,{\[Lambda],chisq}];*)
(*Parms=Table[{pars[[i,1]],pars[[i,2]]},{i,1,Length[pars]}];*)
(*kkcount=0;*)
(*Do[If[Abs[pars[[i,2]]]>10^-2,kkcount=kkcount+1],{i,1,Length[parms]}];*)
(*AppendTo[nopalist,{\[Lambda],kkcount}];*)
(*AppendTo[palist,{\[Lambda],pars[[;;,2]]}];*)
(*Print[\[Lambda]];*)
(*Print[chisq];,{\[Lambda],0,5,.2}]*)
(**)


(* ::Title:: *)
(*Plots*)


(* ::Input:: *)
(*Needs["ErrorBarPlots`"]*)


(* ::Input:: *)
(*chisqr=Table[{chisq[[i,1]],chisq[[i,2]]/(nodat-nopalist[[i,2]])},{i,1,Length[chisq]}];*)
(*plot1=ListPlot[{chisq,chisqwp},Frame->True,PlotRange->{{0.5,5},{100,15000}},PlotLegends->Placed[{"\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\)","\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\)+P"},{Left,Top}],FrameLabel->{{"",""},{"",""}},LabelStyle->12,PlotMarkers->Automatic,Epilog->Text[Style["(a)",FontSize->12],Scaled[{0.9,0.93}],{-1,0}]]*)
(*plot2=ListPlot[chisqr,Frame->True,PlotRange->{{0.5,5},{.901,.929}},FrameLabel->{{"",""},{"",""}},LabelStyle->12,PlotMarkers->Automatic,Epilog->Text[Style["(b)",FontSize->12],Scaled[{0.9,0.93}],{-1,0}]]*)
(*tab1=Table[{3.8,i},{i,-1000,4500}];*)
(*tab2=Table[{3,i},{i,-1000,4500}];*)
(*plot3=Show[ListLogPlot[dataPar,Joined->True,PlotRange->{{0.5,5},{2*10^-5,19}},Frame->True,Epilog->Text[Style["(c)",FontSize->12],Scaled[{0.9,0.9}],{-1,0}],PlotStyle->Table[If[Abs[palist[[16,2,i]]]>10^-2,Red,Gray],{i,1,Length[palist[[20,2]]]}],FrameLabel->{{"",""},{"",""}}],LogPlot[10^-2,{x,0.5,5},PlotStyle->{Thick,Black},PlotRange->{{0.5,5},{2*10^-5,19}}],ListPlot[tab1,Joined->True,PlotStyle->{Thick,Black}],ListPlot[tab2,Joined->True,PlotStyle->{Thick,Black}],LabelStyle->12]*)
(*plot4=Show[ListLogPlot[{aic,aicc,bic},Epilog->Text[Style["(d)",FontSize->12],Scaled[{0.9,0.93}],{-1,0}],Frame->True,PlotRange->{{0.5,5},{3752,4049}},PlotLegends->Placed[{"AICc","AIC","BIC"},{0.9,0.7}],FrameLabel->{{"",""},{"",""}},LabelStyle->12,PlotMarkers->Automatic],ListPlot[tab1,Joined->True,PlotStyle->{Thick,Black}],ListPlot[tab2,Joined->True,PlotStyle->{Thick,Black}]]*)
(*tab=Table[{3,i},{i,230,280,.1}];*)
(*plot5=Show[ErrorListPlot[crossplot,Frame->True,Joined->True,Epilog->Text[Style["(e)",FontSize->12],Scaled[{0.88,0.93}],{-1,0}],PlotRange->{{0.5,5},{239,269}},FrameLabel->{{"",""},{"\[Lambda]",""}},LabelStyle->12],Plot[247.3,{x,3,3.8},PlotStyle->{Thick,Black}],ListPlot[tab2,Joined->True,PlotStyle->{Thick,Black}],ListPlot[tab1,Joined->True,PlotStyle->{Thick,Black}]]*)



