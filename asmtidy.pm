# ------------------------------------------------------------------------------
package asmtidy;

# ------------------------------------------------------------------------------
use Modern::Perl;
use vars qw/$VERSION/;
$VERSION = '1.006';

# ------------------------------------------------------------------------------

=pod
    my $tidy = new asmtidy
    (
        {
            'indent_left' => $indent_left,
            'indent_comma' => $indent_comma,
            'indent_tail_comment' => $indent_tail_comment,
            'indent_operands' => $indent_operands,
            'unaligned_comments' => $unaligned_comments,
            'del_empty_lines' => $del_empty_lines,
            'user_names' => $user_names,
            'log_file' => $log_file
        }
    );
=cut

# ------------------------------------------------------------------------------
sub new {
	my ( $class, $opt ) = @_;

	my $self = {
		name   => __PACKAGE__,
		ver    => $VERSION,
		author => 'Vsevolod Lutovinov <klopp@yandex.ru>'
	};

	%{ $self->{instr} } = map { $_ => 1 } split(
		/[,\s]+/,
		'aaa,aad,aam,aas,adc,add,addpd,addps,addsd,addss,addsubpd,addsubps,aesdec,aesdeclast,aesenc,
aesenclast,aesimc,aeskeygenassist,and,andn,andnpd,andnps,andpd,andps,arpl,bextr,blcfill,blci,blcic,
blcmsk,blcs,blendpd,blendps,blendvpd,blendvps,blsfill,blsi,blsic,blsmk,blsr,bound,bsf,bsr,bswap,bt,
btc,btr,bts,bzhi,call,cbw,cdq,cdqe,clc,cld,clflush,clgi,cli,clts,cmc,cmova,cmovae,cmovb,cmovbe,
cmovc,cmove,cmovg,cmovge,cmovl,cmovle,cmovna,cmovnae,cmovnb,cmovnbe,cmovnc,cmovne,cmovng,cmovnge,
cmovnl,cmovnle,cmovno,cmovnp,cmovns,cmovnz,cmovo,cmovp,cmovpe,cmovpo,cmovs,cmovz,cmp,cmpeqpd,
cmpeqps,cmpeqsd,cmpeqss,cmplepd,cmpleps,cmplesd,cmpless,cmpltpd,cmpltps,cmpltsd,cmpltss,cmpneqpd,
cmpneqps,cmpneqsd,cmpneqss,cmpnlepd,cmpnleps,cmpnlesd,cmpnless,cmpnltpd,cmpnltps,cmpnltsd,cmpnltss,
cmpordpd,cmpordps,cmpordsd,cmpordss,cmppd,cmpps,cmps,cmpsb,cmpsd,cmpsq,cmpss,cmpsw,cmpunordpd,
cmpunordps,cmpunordsd,cmpunordss,cmpxchg,cmpxchg16b,cmpxchg8b,comisd,comiss,cpuid,cqo,crc32,
cvtdq2pd,cvtdq2ps,cvtpd2dq,cvtpd2pi,cvtpd2ps,cvtpi2pd,cvtpi2ps,cvtps2dq,cvtps2pd,cvtps2pi,cvtsd2si,
cvtsd2ss,cvtsi2sd,cvtsi2ss,cvtss2sd,cvtss2si,cvttpd2dq,cvttpd2pi,cvttps2dq,cvttps2pi,cvttsd2si,
cvttss2si,cwd,cwde,daa,das,dec,div,divpd,divps,divsd,divss,dppd,dpps,emms,enter,extractps,extrq,
f2xm1,fabs,fadd,faddp,fbld,fbstp,fchs,fclex,fcmovb,fcmovbe,fcmove,fcmovnb,fcmovnbe,fcmovne,fcmovnu,
fcmovu,fcom,fcomi,fcomip,fcomp,fcompp,fcos,fdecstp,fdisi,fdiv,fdivp,fdivr,fdivrp,femms,feni,ffree,
ffreep,fiadd,ficom,ficomp,fidiv,fidivr,fild,fimul,fincstp,finit,fist,fistp,fisttp,fisub,fisubr,fld,
fld1,fldcw,fldenv,fldenvd,fldenvw,fldl2e,fldl2t,fldlg2,fldln2,fldpi,fldz,fmul,fmulp,fnclex,fndisi,
fneni,fninit,fnop,fnsave,fnsaved,fnsavew,fnstcw,fnstenv,fnstenvd,fnstenvw,fnstsw,fpatan,fprem,
fprem1,fptan,frndint,frstor,frstord,frstorw,fsave,fsaved,fsavew,fscale,fsetpm,fsin,fsincos,fsqrt,
fst,fstcw,fstenv,fstenvd,fstenvw,fstp,fstsw,fsub,fsubp,fsubr,fsubrp,ftst,fucom,fucomi,fucomip,
fucomp,fucompp,fwait,fxam,fxch,fxrstor,fxrstor64,fxsave,fxsave64,fxtract,fyl2x,fyl2xp1,getsec,
haddpd,haddps,hlt,hsubpd,hsubps,idiv,imul,in,inc,insb,insd,insertps,insertq,insw,int,into,invd,
invept,invlpg,invlpga,invpcid,invvpid,iret,iretd,iretq,ja,jae,jb,jbe,jc,jcxz,je,jecxz,jg,jge,jl,jle,
jmp,jna,jnae,jnb,jnbe,jnc,jne,jng,jnge,jnl,jnle,jno,jnp,jns,jnz,jo,jp,jpe,jpo,jrcxz,js,jz,lahf,lar,
lddqu,ldmxcsr,lds,lea,leave,les,lfence,lfs,lgdt,lgs,lidt,lldt,llwpcb,lmsw,lock,lods,lodsb,lodsd,
lodsq,lodsw,loop,loope,loopne,lsl,lss,ltr,lwpins,lwpval,lzcnt,maskmovdqu,maskmovq,maxpd,maxps,maxsd,
maxss,mfence,minpd,minps,minsd,minss,monitor,montmul,mov,movapd,movaps,movbe,movd,movddup,movdq2q,
movdqa,movdqu,movhlps,movhpd,movhps,movlhps,movlpd,movlps,movmskpd,movmskps,movntdq,movntdqa,movnti,
movntpd,movntps,movntq,movntsd,movntss,movq,movq2dq,movs,movsb,movsd,movshdup,movsldup,movsq,movss,
movsw,movsx,movsxd,movupd,movups,movzx,mpsadbw,mul,mulpd,mulps,mulsd,mulss,mulx,mwait,neg,nop,not,
or,orpd,orps,out,outsb,outsd,outsw,pabsb,pabsd,pabsw,packssdw,packsswb,packusdw,packuswb,paddb,
paddd,paddq,paddsb,paddsiw,paddsw,paddusb,paddusw,paddw,palignr,pand,pandn,paveb,pavgb,pavgusb,
pavgw,pblendvb,pblendw,pclmulqdq,pcmpeqb,pcmpeqd,pcmpeqq,pcmpeqw,pcmpestri,pcmpestrm,pcmpgtb,
pcmpgtd,pcmpgtq,pcmpgtw,pcmpistri,pcmpistrm,pdep,pdistib,pext,pextrb,pextrd,pextrq,pextrw,pf2id,
pf2iw,pfacc,pfadd,pfcmpeq,pfcmpge,pfcmpgt,pfmax,pfmin,pfmul,pfnacc,pfpnacc,pfrcp,pfrcpit1,pfrcpit2,
pfrcpv,pfrsqit1,pfrsqrt,pfrsqrtv,pfsub,pfsubr,phaddd,phaddsw,phaddw,phminposuw,phsubd,phsubsw,
phsubw,pi2fd,pi2fw,pinsrb,pinsrd,pinsrq,pinsrw,pmachriw,pmaddubsw,pmaddwd,pmagw,pmaxsb,pmaxsd,
pmaxsw,pmaxub,pmaxud,pmaxuw,pminsb,pminsd,pminsw,pminub,pminud,pminuw,pmovmskb,pmovsxbd,pmovsxbq,
pmovsxbw,pmovsxdq,pmovsxwd,pmovsxwq,pmovzxbd,pmovzxbq,pmovzxbw,pmovzxdq,pmovzxwd,pmovzxwq,pmuldq,
pmulhriw,pmulhrsw,pmulhrw,pmulhuw,pmulhw,pmulld,pmullw,pmuludq,pmvgezb,pmvlzb,pmvnzb,pmvzb,pop,popa,
popad,popcnt,popf,popfd,popfq,por,prefetch,prefetchnta,prefetcht0,prefetcht1,prefetcht2,prefetchw,
psadbw,pshufb,pshufd,pshufhw,pshuflw,pshufw,psignb,psignd,psignw,pslld,pslldq,psllq,psllw,psrad,
psraw,psrld,psrldq,psrlq,psrlw,psubb,psubd,psubq,psubsb,psubsiw,psubsw,psubusb,psubusw,psubw,pswapd,
ptest,punpckhbw,punpckhdq,punpckhqdq,punpckhwd,punpcklbw,punpckldq,punpcklqdq,punpcklwd,push,pusha,
pushad,pushf,pushfd,pushfq,pxor,rcl,rcpps,rcpss,rcr,rdfsbase,rdgsbase,rdmsr,rdpmc,rdrand,rdtsc,
rdtscp,rep,repe,repne,repnz,repz,ret,rol,ror,rorx,roundpd,roundps,roundsd,roundss,rsm,rsqrtps,
rsqrtss,sahf,sal,salc,sar,sarx,sbb,scas,scasb,scasd,scasq,scasw,seta,setae,setb,setbe,setc,sete,
setg,setge,setl,setle,setna,setnae,setnb,setnbe,setnc,setne,setng,setnge,setnl,setnle,setno,setnp,
setns,setnz,seto,setp,setpe,setpo,sets,setz,sfence,sgdt,shl,shld,shlx,shr,shrd,shrx,shufpd,shufps,
sidt,skinit,sldt,slwpcb,smsw,sqrtpd,sqrtps,sqrtsd,sqrtss,stc,std,stgi,sti,stmxcsr,stos,stosb,stosd,
stosq,stosw,str,sub,subpd,subps,subsd,subss,swapgs,syscall,sysenter,sysexit,sysret,t1mskc,test,
tzcnt,tzmsk,ucomisd,ucomiss,ud2,unpckhpd,unpckhps,unpcklpd,unpcklps,vaddpd,vaddps,vaddsd,vaddss,
vaddsubpd,vaddsubps,vaesdec,vaesdeclast,vaesenc,vaesenclast,vaesimc,vaeskeygenassist,vandnpd,
vandnps,vandpd,vandps,vblendpd,vblendps,vblendvpd,vblendvps,vbroadcastf128,vbroadcasti128,
vbroadcastsd,vbroadcastss,vcmpeq_ospd,vcmpeq_osps,vcmpeq_ossd,vcmpeq_osss,vcmpeq_uqpd,vcmpeq_uqps,
vcmpeq_uqsd,vcmpeq_uqss,vcmpeq_uspd,vcmpeq_usps,vcmpeq_ussd,vcmpeq_usss,vcmpeqpd,vcmpeqps,vcmpeqsd,
vcmpeqss,vcmpfalse_ospd,vcmpfalse_osps,vcmpfalse_ossd,vcmpfalse_osss,vcmpfalsepd,vcmpfalseps,
vcmpfalsesd,vcmpfalsess,vcmpge_oqpd,vcmpge_oqps,vcmpge_oqsd,vcmpge_oqss,vcmpgepd,vcmpgeps,vcmpgesd,
vcmpgess,vcmpgt_oqpd,vcmpgt_oqps,vcmpgt_oqsd,vcmpgt_oqss,vcmpgtpd,vcmpgtps,vcmpgtsd,vcmpgtss,
vcmple_oqpd,vcmple_oqps,vcmple_oqsd,vcmple_oqss,vcmplepd,vcmpleps,vcmplesd,vcmpless,vcmplt_oqpd,
vcmplt_oqps,vcmplt_oqsd,vcmplt_oqss,vcmpltpd,vcmpltps,vcmpltsd,vcmpltss,vcmpneq_oqpd,vcmpneq_oqps,
vcmpneq_oqsd,vcmpneq_oqss,vcmpneq_ospd,vcmpneq_osps,vcmpneq_ossd,vcmpneq_osss,vcmpneq_uspd,
vcmpneq_usps,vcmpneq_ussd,vcmpneq_usss,vcmpneqpd,vcmpneqps,vcmpneqsd,vcmpneqss,vcmpnge_uqpd,
vcmpnge_uqps,vcmpnge_uqsd,vcmpnge_uqss,vcmpngepd,vcmpngeps,vcmpngesd,vcmpngess,vcmpngt_uqpd,
vcmpngt_uqps,vcmpngt_uqsd,vcmpngt_uqss,vcmpngtpd,vcmpngtps,vcmpngtsd,vcmpngtss,vcmpnle_uqpd,
vcmpnle_uqps,vcmpnle_uqsd,vcmpnle_uqss,vcmpnlepd,vcmpnleps,vcmpnlesd,vcmpnless,vcmpnlt_uqpd,
vcmpnlt_uqps,vcmpnlt_uqsd,vcmpnlt_uqss,vcmpnltpd,vcmpnltps,vcmpnltsd,vcmpnltss,vcmpord_spd,
vcmpord_sps,vcmpord_ssd,vcmpord_sss,vcmpordpd,vcmpordps,vcmpordsd,vcmpordss,vcmppd,vcmpps,vcmpsd,
vcmpss,vcmptrue_uspd,vcmptrue_usps,vcmptrue_ussd,vcmptrue_usss,vcmptruepd,vcmptrueps,vcmptruesd,
vcmptruess,vcmpunord_spd,vcmpunord_sps,vcmpunord_ssd,vcmpunord_sss,vcmpunordpd,vcmpunordps,
vcmpunordsd,vcmpunordss,vcomisd,vcomiss,vcvtdq2pd,vcvtdq2ps,vcvtpd2dq,vcvtpd2pi,vcvtpd2ps,vcvtph2ps,
vcvtpi2pd,vcvtpi2ps,vcvtps2dq,vcvtps2pd,vcvtps2ph,vcvtps2pi,vcvtsd2si,vcvtsd2ss,vcvtsi2sd,vcvtsi2ss,
vcvtss2sd,vcvtss2si,vcvttpd2dq,vcvttpd2pi,vcvttps2dq,vcvttps2pi,vcvttsd2si,vcvttss2si,vdivpd,vdivps,
vdivsd,vdivss,vdppd,vdpps,verr,verw,vextractf128,vextracti128,vextractps,vfmadd132pd,vfmadd132ps,
vfmadd132sd,vfmadd132ss,vfmadd213pd,vfmadd213ps,vfmadd213sd,vfmadd213ss,vfmadd231pd,vfmadd231ps,
vfmadd231sd,vfmadd231ss,vfmaddpd,vfmaddps,vfmaddsd,vfmaddss,vfmaddsub132pd,vfmaddsub132ps,
vfmaddsub213pd,vfmaddsub213ps,vfmaddsub231pd,vfmaddsub231ps,vfmaddsubpd,vfmaddsubps,vfmsub132pd,
vfmsub132ps,vfmsub132sd,vfmsub132ss,vfmsub213pd,vfmsub213ps,vfmsub213sd,vfmsub213ss,vfmsub231pd,
vfmsub231ps,vfmsub231sd,vfmsub231ss,vfmsubadd132pd,vfmsubadd132ps,vfmsubadd213pd,vfmsubadd213ps,
vfmsubadd231pd,vfmsubadd231ps,vfmsubaddpd,vfmsubaddps,vfmsubpd,vfmsubps,vfmsubsd,vfmsubss,
vfnmadd132pd,vfnmadd132ps,vfnmadd132sd,vfnmadd132ss,vfnmadd213pd,vfnmadd213ps,vfnmadd213sd,
vfnmadd213ss,vfnmadd231pd,vfnmadd231ps,vfnmadd231sd,vfnmadd231ss,vfnmsub132pd,vfnmsub132ps,
vfnmsub132sd,vfnmsub132ss,vfnmsub213pd,vfnmsub213ps,vfnmsub213sd,vfnmsub213ss,vfnmsub231pd,
vfnmsub231ps,vfnmsub231sd,vfnmsub231ss,vfrczpd,vfrczps,vfrczsd,vfrczss,vgatherdpd,vgatherdps,
vgatherdq,vgatherqpd,vgatherqps,vgatherqq,vhaddpd,vhaddps,vhsubpd,vhsubps,vinsertf128,vinserti128,
vinsertps,vlddqu,vldmxcsr,vmaskmovdqu,vmaskmovpd,vmaskmovps,vmaxpd,vmaxps,vmaxsd,vmaxss,vmcall,
vmclear,vminpd,vminps,vminsd,vminss,vmlaunch,vmload,vmmcall,vmovapd,vmovaps,vmovd,vmovddup,vmovdqa,
vmovdqu,vmovhlps,vmovhpd,vmovhps,vmovlhps,vmovlpd,vmovlps,vmovmskpd,vmovmskps,vmovntdq,vmovntdqa,
vmovntpd,vmovntps,vmovq,vmovsd,vmovshdup,vmovsldup,vmovss,vmovupd,vmovups,vmpsadbw,vmptrld,vmptrst,
vmread,vmresume,vmrun,vmsave,vmulpd,vmulps,vmulsd,vmulss,vmwrite,vmxoff,vmxon,vnfmaddpd,vnfmaddps,
vnfmaddsd,vnfmaddss,vnfmsubpd,vnfmsubps,vnfmsubsd,vnfmsubss,vorpd,vorps,vpabsb,vpabsd,vpabsw,
vpackssdw,vpacksswb,vpackusdw,vpackuswb,vpaddb,vpaddd,vpaddq,vpaddsb,vpaddsw,vpaddusb,vpaddusw,
vpaddw,vpalignr,vpand,vpandn,vpavgb,vpavgw,vpblendd,vpblendw,vpblendwb,vpbroadcastb,vpbroadcastd,
vpbroadcastq,vpbroadcastw,vpclmulqdq,vpcmov,vpcmpeqb,vpcmpeqd,vpcmpeqq,vpcmpeqw,vpcmpestri,
vpcmpestrm,vpcmpgtb,vpcmpgtd,vpcmpgtq,vpcmpgtw,vpcmpistri,vpcmpistrm,vpcomb,vpcomd,vpcomq,vpcomub,
vpcomud,vpcomuq,vpcomuw,vpcomw,vperlil2pd,vperm2f128,vperm2i128,vpermd,vpermil2ps,vpermilpd,
vpermilps,vpermpd,vpermps,vpermq,vpextrb,vpextrd,vpextrq,vpextrw,vpgatherdd,vpgatherqd,vphaddbd,
vphaddbq,vphaddbw,vphaddd,vphadddq,vphaddsw,vphaddubd,vphaddubq,vphaddubw,vphaddudq,vphadduwd,
vphadduwq,vphaddw,vphaddwd,vphaddwq,vphminposuw,vphsubbw,vphsubd,vphsubdq,vphsubsw,vphsubw,vphsubwd,
vpinsrb,vpinsrd,vpinsrq,vpinsrw,vpmacsdd,vpmacsdqh,vpmacsdql,vpmacssdd,vpmacssdqh,vpmacssdql,
vpmacsswd,vpmacssww,vpmacswd,vpmacsww,vpmadcsswd,vpmadcswd,vpmaddubsw,vpmaddwd,vpmaskmovd,
vpmaskmovq,vpmaxsb,vpmaxsd,vpmaxsw,vpmaxub,vpmaxud,vpmaxuw,vpminsb,vpminsd,vpminsw,vpminub,vpminud,
vpminuw,vpmovmskb,vpmovsxbd,vpmovsxbq,vpmovsxbw,vpmovsxdq,vpmovsxwd,vpmovsxwq,vpmovzxbd,vpmovzxbq,
vpmovzxbw,vpmovzxdq,vpmovzxwd,vpmovzxwq,vpmuldq,vpmulhrsw,vpmulhuw,vpmulhw,vpmulld,vpmullw,vpmuludq,
vpor,vpperm,vprotb,vprotd,vprotq,vprotw,vpsadbw,vpshab,vpshad,vpshaq,vpshaw,vpshlb,vpshld,vpshlq,
vpshlw,vpshufb,vpshufd,vpshufhw,vpshuflw,vpsignb,vpsignd,vpsignw,vpslld,vpslldq,vpsllq,vpsllvd,
vpsllvq,vpsllw,vpsrad,vpsravd,vpsraw,vpsrld,vpsrldq,vpsrlq,vpsrlvd,vpsrlw,vpsubb,vpsubd,vpsubq,
vpsubsb,vpsubsw,vpsubusb,vpsubusw,vpsubw,vptest,vpunpckhbw,vpunpckhdq,vpunpckhqdq,vpunpckhwd,
vpunpcklbw,vpunpckldq,vpunpcklqdq,vpunpcklwd,vpxor,vrcpps,vrcpss,vroundpd,vroundps,vroundsd,
vroundss,vrsqrtps,vrsqrtss,vshufpd,vshufps,vsqrtpd,vsqrtps,vsqrtsd,vsqrtss,vstmxcsr,vsubpd,vsubps,
vsubsd,vsubss,vtestpd,vtestps,vucomisd,vucomiss,vunpckhpd,vunpckhps,vunpcklpd,vunpcklps,vxorpd,
vxorps,vzeroall,vzeroupper,wbinvd,wrfsbase,wrgsbase,wrmsr,xadd,xchg,xcryptcbc,xcryptcfb,xcryptctr,
xcryptecb,xcryptofb,xgetbv,xlatb,xor,xorpd,xorps,xrstor,xsave,xsaveopt,xsetbv,xsha1,xsha256,xstore'
	);

	$self->{cmt_rx} = qr/^(.+?)(\s*;\s*)(?=(?:[^"']|["'][^'"]*['"])*$)(.*)/o;
	$self->{comma_rx} = qr/,(?=(?:[^"']|["'][^'"]*['"])*$)/;
	$self->{label_rx} = qr/^([\?\$\@\w][\?\$\@\w\d]+\:)(.+)/o;

	bless( $self, $class );
	$self->set_opt($opt);
	$self;
}

# ------------------------------------------------------------------------------
sub set_opt {
	my ( $self, $opt ) = @_;

	$self->{opt} = $opt;

	%{ $self->{user_ids} }
		= map { lc($_) => 1 } split( /[\s,;]+/, $self->{opt}->{user_names} )
		if $self->{opt}->{user_names};

	$self->{opt}->{indent_left}
		= defined $self->{opt}->{indent_left}
		? ( $self->{opt}->{indent_left} =~ /^(\d+)$/ ? $1 : 4 )
		: 4;
	$self->{opt}->{indent_comma}
		= $self->{opt}->{indent_comma} =~ /^(\d+)$/ ? $1 : 0;
	$self->{opt}->{indent_tail_comment}
		= $self->{opt}->{indent_tail_comment}
		? ( $self->{opt}->{indent_tail_comment} =~ /^(\d+)$/ ? $1 : 1 )
		: 1;

	$self->{opt}->{del_empty_lines} ||= 'no';
	$self->{opt}->{del_empty_lines} = 'yes'
		unless $self->{opt}->{del_empty_lines} =~ /^(yes|no|all)$/i;

	die "indent_operands => {N|tabN}, got '"
		. $self->{opt}->{indent_operands} . "'!\n"
		if defined $self->{opt}->{indent_operands}
		&& $self->{opt}->{indent_operands} !~ /^(\d+)|tab(\d+)$/;
	$self->{opt}->{indent_operands} = 1
		unless defined $self->{opt}->{indent_operands};

	return $self;
}

# ------------------------------------------------------------------------------
sub tidy_file {
	my ( $self, $file ) = @_;

	open my $f, '<:encoding(utf8)', $file || die "Can not read '$file': $!\n";
	$self->{lines}  = ();
	$self->{lastcr} = 0;
	$self->_format_line($_) while <$f>;
	close $f;
	$self->_out();
}

# ------------------------------------------------------------------------------
sub tidy_content {
	my ( $self, $content ) = @_;

	$self->{lastcr} = 0;
	$self->{lines}  = ();
	$self->_format_line($_)
		for (
		ref $content eq 'ARRAY' ? @{$content} : split( /\n/, $content ) );
	$self->_out();
}

# ------------------------------------------------------------------------------
sub author {
	my ($self) = @_;
	return $self->{author};
}

# ------------------------------------------------------------------------------
sub ver {
	my ($self) = @_;
	return $self->{ver};
}

# ------------------------------------------------------------------------------
sub name {
	my ($self) = @_;
	return $self->{name};
}

# ------------------------------------------------------------------------------
sub id {
	my ($self) = @_;
	return
		  $self->{name} . ' ver '
		. $self->{ver}
		. ', (C) '
		. $self->{author};
}

# ------------------------------------------------------------------------------
sub _out {
	my ($self) = @_;
	$self->_format_comments();

	my $copy = '; ' . $self->id();
	$copy =~ s{<}{&lt;}g;
	$copy =~ s{>}{&gt;}g;
	unshift @{ $self->{lines} }, $copy if $ENV{HTTP_HOST};

	return join( "\n", map { $_->[0] } @{ $self->{lines} } );
}

# ------------------------------------------------------------------------------
sub _format_line {
	my ( $self, $line ) = @_;

	chomp $line;

	return if $line =~ /^; asmtidy\.pm v \d+\.\d+$/;

	if ( $line =~ /^\s*$/ ) {
		return if $self->{opt}->{del_empty_lines} eq 'all';
		$self->{lastcr} = 0 unless $self->{opt}->{del_empty_lines} eq 'yes';
		push( @{ $self->{lines} }, [''] ) if $self->{lastcr} < 1;
		$self->{lastcr}++;
		return;
	}
	$self->{lastcr} = 0;

	if ( $line =~ /^(\s*)(;.*)$/ ) {
		if ( $1 ne '' ) {
			if ( $self->{opt}->{unaligned_comments} eq 'left' ) {
				$line = $2;
			}
			elsif ( $self->{opt}->{unaligned_comments} eq 'right' ) {
				$line = ( ' ' x $self->{opt}->{indent_left} ) . $2;
			}
			else {
			}
		}

		push( @{ $self->{lines} }, [$line] );
		return;
	}

	if ( $line =~ /$self->{label_rx}/ ) {
		my ( $label, $tail ) = ( $1, $2 );
		$self->_format_line($label);
		$self->_format_line($tail);
		return;
	}

	my $comment = '';

	$line =~ /$self->{cmt_rx}/ and $line = $1, $comment = "$2$3";
	$line =~ s/^\s+|\s+$//gs;
	$comment =~ s/^\s+|\s+$//gs;

	my ( $first, $last ) = ( '', '' );

	$first = $line;

	$first = $1, $last = $2
		if $line =~ /^([^\s]+)\s+([^\s].*)$/;

	$first =~ s/^\s+|\s+$//g;
	$last =~ s/^\s+|\s+$//g;

	if ( $self->{instr}->{ lc($first) } || $self->{user_ids}->{ lc($first) } )
	{
		if ( $last && $self->{opt}->{indent_operands} =~ /^tab(\d+)$/ ) {
			my $max    = $1;
			my $length = length $first;
			my $sp     = $max - $length;
			$sp = 1 if $sp <= 0;
			$last = ( ' ' x $sp ) . $last if $sp > 0;
		}
		elsif ( $self->{opt}->{indent_operands} =~ /^\d+$/ ) {
			$last = ( ' ' x $self->{opt}->{indent_operands} ) . $last;
		}

		if ( $last ne '' ) {
			$last =~ /^(.+?)\s*,(.+?)$/;
			if ($2) {
				my ( $part1, $part2 ) = ( $1, $2 );
				$part1 =~ s/\s+$//g;
				my @parts = split( $self->{comma_rx}, $part2 );
				s/^\s+|\s+$//g for @parts;
				unshift @parts, $part1;
				$last = join(
					',' . ( ' ' x $self->{opt}->{indent_comma} ),
					@parts
				);
			}
		}

		$last =~ s/\s+$//g;
		push(
			@{ $self->{lines} },
			[   ( ' ' x $self->{opt}->{indent_left} ) . $first . $last,
				$comment
			]
		);
		return;
	}
	else {
	}

	push( @{ $self->{lines} }, [ $line, $comment ] );
}

# ------------------------------------------------------------------------------
sub _format_comments {
	my ($self) = @_;

	my $max = 0;
	my %clines;
	my $idx = 0;

	foreach my $line ( @{ $self->{lines} } ) {
		if ( $line->[0] !~ /^\s+$/ && $line->[1] ) {
			my $length = length $line->[0];
			$max = $length if $max < $length;
			$clines{$idx} = $length;
		}
		$idx++;
	}

	for $idx ( 0 .. $#{ $self->{lines} } ) {
		next unless $clines{$idx};

		my $sp = $max - $clines{$idx} + $self->{opt}->{indent_tail_comment};
		$self->{lines}->[$idx]->[0]
			.= ( ' ' x $sp ) . $self->{lines}->[$idx]->[1];
		undef $clines{$idx};
		undef $self->{lines}->[$idx]->[1];
	}
}

# ------------------------------------------------------------------------------
sub _log {
	my $self = shift;
	if ( $self->{opt}->{log_file} ) {
		open my $f, '>>' . $self->{opt}->{log_file};
		if ($f) {
			print $f ( join( "\n", @_ ) ) . "\n";
			close $f;
		}
	}
}

# ------------------------------------------------------------------------------
1;
