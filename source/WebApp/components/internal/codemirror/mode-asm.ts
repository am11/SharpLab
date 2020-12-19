// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE
import CodeMirror from 'codemirror';

CodeMirror.defineMode('asm', () => {
    const grammar = {
        builtin: new RegExp('^(?:' + [
            /* spellchecker: disable */
            'aa[adms]', 'adc', 'add', 'and', 'arpl', 'bound', 'bsf', 'bsr', 'bswap', 'bt', 'bt[crs]',
            'call', 'cbw', 'cdq', 'cl[cdi]', 'clts', 'cmc', conditional('cmov'), 'cmp', 'cmps[bdw]', 'cmpxchg', 'cmpxchg8b', 'cpuid', 'cwd', 'cwde',
            'daa', 'das', 'dec', 'div', 'enter', 'esc', 'hlt',
            'idiv', 'imul', 'in', 'inc', 'ins', 'insd', 'int', 'int[13o]', 'invd', 'invlpg', 'iret', 'iret[df]',
            conditional('j'), 'jecxz', 'jcxz', 'jmp',
            'lahf', 'lar', 'lds', 'lea', 'leave', 'les', 'l[gil]dt', 'lfs', 'lgs', 'lmsw', 'loadall', 'lock', 'lods[bdw]', 'loop', 'loop[dw]?', 'loopn?[ez][dw]?', 'lsl', 'lss', 'ltr',
            'mov', 'movs[bdw]', 'mov[sz]x', 'movsxd', 'mul', 'neg', 'nop', 'not', 'or', 'out', 'outsd?', 'pop', 'pop[af]d?', 'push', 'push[af]d?',
            'rcl', 'rcr', 'rdmsr', 'rdpmc', 'rdtsc', 'rep', 'repn?[ez]', 'ret', 'retn', 'retf', 'rol', 'ror', 'rsm',
            'sahf', 'sal', 'sar', 'sbb', 'scas[bdw]', conditional('set'), 's[gil]dt', 'shld?', 'shrd?', 'smsw', 'st[cdi]', 'stos[bdw]', 'str',
            'sub', 'syscall', 'sysenter', 'sysexit', 'sysret', 'test', 'ud2', 'vcvtsi2ss', 'vcvtss2sd', 'verr', 'verw',
            'vmovdqu', 'vmovdqu8', 'vmovdqu16', 'vmovdqu32', 'vmovsd', 'vmovss', 'vmulss', 'vucomisd', 'vxorps', 'vzeroupper',
            'wait', 'wbinvd', 'wrmsr',
            'xadd', 'xchg', 'xlat', 'xor', 'xorps', 'addpd', 'addps', 'addsd', 'addss', 'addsubpd', 'addsubps', 'aesdec', 'aesdeclast', 'aesenc',
            'aesenclast', 'aesimc', 'aeskeygenassist', 'align', 'andn', 'andnpd', 'andnps', 'andpd', 'andps', 'bextr', 'blendpd', 'blendps',
            'blendvpd', 'blendvps', 'blsi', 'blsmsk', 'blsr', 'broadcastf128', 'broadcasti128', 'broadcastsd', 'broadcastss',
            'bzhi', 'cmppd', 'cmpps', 'cmpss', 'comisd', 'comiss', 'crc32', 'cvtdq2pd', 'cvtdq2ps', 'cvtpd2dq', 'cvtpd2pi',
            'cvtpd2ps', 'cvtpi2pd', 'cvtpi2ps', 'cvtps2dq', 'cvtps2pd', 'cvtps2pi', 'cvtsd2si', 'cvtsd2ss', 'cvtsi2sd', 'cvtsi2ss',
            'cvtss2sd', 'cvtss2si', 'cvttpd2dq', 'cvttpd2pi', 'cvttps2dq', 'cvttps2pi', 'cvttsd2si', 'cvttss2si', 'divpd', 'divps',
            'divsd', 'divss', 'dppd', 'dpps', 'extractf128', 'extracti128', 'extractps', 'fld', 'fmadd132pd', 'fmadd132ps', 'fmadd132sd',
            'fmadd132ss', 'fmadd213pd', 'fmadd213ps', 'fmadd213sd', 'fmadd213ss', 'fmadd231pd', 'fmadd231ps', 'fmadd231sd', 'fmadd231ss',
            'fmaddsub132pd', 'fmaddsub132ps', 'fmaddsub213pd', 'fmaddsub213ps', 'fmaddsub231pd', 'fmaddsub231ps', 'fmsub132pd', 'fmsub132ps',
            'fmsub132sd', 'fmsub132ss', 'fmsub213pd', 'fmsub213ps', 'fmsub213sd', 'fmsub213ss', 'fmsub231pd', 'fmsub231ps', 'fmsub231sd',
            'fmsub231ss', 'fmsubadd132pd', 'fmsubadd132ps', 'fmsubadd213pd', 'fmsubadd213ps', 'fmsubadd231pd', 'fmsubadd231ps', 'fnmadd132pd',
            'fnmadd132ps', 'fnmadd132sd', 'fnmadd132ss', 'fnmadd213pd', 'fnmadd213ps', 'fnmadd213sd', 'fnmadd213ss', 'fnmadd231pd', 'fnmadd231ps',
            'fnmadd231sd', 'fnmadd231ss', 'fnmsub132pd', 'fnmsub132ps', 'fnmsub132sd', 'fnmsub132ss', 'fnmsub213pd', 'fnmsub213ps', 'fnmsub213sd',
            'fnmsub213ss', 'fnmsub231pd', 'fnmsub231ps', 'fnmsub231sd', 'fnmsub231ss', 'fstp', 'gatherdpd', 'gatherdps', 'gatherqpd', 'gatherqps',
            'haddpd', 'haddps', 'hsubpd', 'hsubps', 'insertf128', 'inserti128', 'insertps', 'lddqu', 'lfence', 'lzcnt', 'maskmovdqu', 'maskmovpd',
            'maskmovps', 'maxpd', 'maxps', 'maxsd', 'maxss', 'mfence', 'minpd', 'minps', 'minsd', 'minss', 'movapd', 'movaps', 'movd', 'movddup',
            'movdqa', 'movdqu', 'movhlps', 'movhpd', 'movhps', 'movlhps', 'movlpd', 'movlps', 'movmskpd', 'movmskps', 'movntdq', 'movntdqa', 'movnti',
            'movntpd', 'movntps', 'movq', 'movshdup', 'movsldup', 'movsq', 'movss', 'movupd', 'movups', 'mpsadbw', 'mulpd', 'mulps', 'mulsd', 'mulss',
            'mulx', 'orpd', 'orps', 'pabsb', 'pabsd', 'pabsw', 'packssdw', 'packsswb', 'packusdw', 'packuswb', 'paddb', 'paddd', 'paddq', 'paddsb',
            'paddsw', 'paddusb', 'paddusw', 'paddw', 'palignr', 'pand', 'pandn', 'pavgb', 'pavgw', 'pblendd', 'pblendvb', 'pblendw', 'pbroadcastb',
            'pbroadcastd', 'pbroadcastq', 'pbroadcastw', 'pclmulqdq', 'pcmpeqb', 'pcmpeqd', 'pcmpeqq', 'pcmpeqw', 'pcmpgtb', 'pcmpgtd', 'pcmpgtq',
            'pcmpgtw', 'pdep', 'perm2f128', 'perm2i128', 'permd', 'permilpd', 'permilpdvar', 'permilps', 'permilpsvar', 'permpd', 'permps', 'permq',
            'pext', 'pextrb', 'pextrd', 'pextrq', 'pextrw', 'pgatherdd', 'pgatherdq', 'pgatherqd', 'pgatherqq', 'phaddd', 'phaddsw', 'phaddw', 'phminposuw',
            'phsubd', 'phsubsw', 'phsubw', 'pinsrb', 'pinsrd', 'pinsrq', 'pinsrw', 'pmaddubsw', 'pmaddwd', 'pmaskmovd', 'pmaskmovq', 'pmaxsb', 'pmaxsd',
            'pmaxsw', 'pmaxub', 'pmaxud', 'pmaxuw', 'pminsb', 'pminsd', 'pminsw', 'pminub', 'pminud', 'pminuw', 'pmovmskb', 'pmovsxbd', 'pmovsxbq',
            'pmovsxbw', 'pmovsxdq', 'pmovsxwd', 'pmovsxwq', 'pmovzxbd', 'pmovzxbq', 'pmovzxbw', 'pmovzxdq', 'pmovzxwd', 'pmovzxwq', 'pmuldq', 'pmulhrsw',
            'pmulhuw', 'pmulhw', 'pmulld', 'pmullw', 'pmuludq', 'popcnt', 'por', 'prefetchnta', 'prefetcht0', 'prefetcht1', 'prefetcht2', 'psadbw', 'pshufb',
            'pshufd', 'pshufhw', 'pshuflw', 'psignb', 'psignd', 'psignw', 'pslld', 'pslldq', 'psllq', 'psllvd', 'psllvq', 'psllw', 'psrad', 'psravd', 'psraw',
            'psrld', 'psrldq', 'psrlq', 'psrlvd', 'psrlvq', 'psrlw', 'psubb', 'psubd', 'psubq', 'psubsb', 'psubsw', 'psubusb', 'psubusw', 'psubw', 'ptest',
            'punpckhbw', 'punpckhdq', 'punpckhqdq', 'punpckhwd', 'punpcklbw', 'punpckldq', 'punpcklqdq', 'punpcklwd', 'pxor', 'rcpps', 'rcpss', 'rex.jmp', 'rorx',
            'roundpd', 'roundps', 'roundsd', 'roundss', 'rsqrtps', 'rsqrtss', 'sfence', 'shufpd', 'shufps', 'sqrtpd', 'sqrtps', 'sqrtsd', 'sqrtss', 'stosq',
            'subpd', 'subps', 'subsd', 'subss', 'testpd', 'testps', 'tzcnt', 'ucomisd', 'ucomiss', 'unpckhpd', 'unpckhps', 'unpcklpd', 'unpcklps', 'xorpd',
            'zeroupper', 'cmpxchg486', 'emms', 'f2xm1', 'fabs', 'fadd', 'faddp', 'fbld', 'fbstp', 'fchs', 'fclex', 'fcmovb', 'fcmovbe', 'fcmove', 'fcmovnb',
            'fcmovnbe', 'fcmovne', 'fcmovnu', 'fcmovu', 'fcom', 'fcomi', 'fcomip', 'fcomp', 'fcompp', 'fcos', 'fdecstp', 'fdisi', 'fdiv', 'fdivp',
            'fdivr', 'fdivrp', 'femms', 'feni', 'ffree', 'fiadd', 'ficom', 'ficomp', 'fidiv', 'fidivr', 'fild', 'fimul', 'fincstp', 'finit', 'fist',
            'fistp', 'fisttp', 'fisub', 'fisubr', 'fld1', 'fldcw', 'fldenv', 'fldl2e', 'fldl2t', 'fldlg2', 'fldln2', 'fldpi', 'fldz', 'fmul', 'fmulp',
            'fnclex', 'fndisi', 'fneni', 'fninit', 'fnop', 'fnsave', 'fnstcw', 'fnstenv', 'fnstsw', 'fpatan', 'fprem', 'fprem1', 'fptan', 'frndint',
            'frstor', 'fsave', 'fscale', 'fsetpm', 'fsin', 'fsincos', 'fsqrt', 'fst', 'fstcw', 'fstenv', 'fstsw', 'fsub', 'fsubp', 'fsubr', 'fsubrp',
            'ftst', 'fucom', 'fucomi', 'fucomip', 'fucomp', 'fucompp', 'fwait', 'fxam', 'fxch', 'fxtract', 'fyl2x', 'fyl2xp1', 'ibts', 'icebp', 'insb',
            'insw', 'int01', 'int03', 'iretw', 'lcall', 'ljmp', 'loadall286', 'monitor', 'mwait', 'outsb', 'outsw', 'paddsiw', 'paveb', 'pavgusb',
            'pdistib', 'pf2id', 'pfacc', 'pfadd', 'pfcmpeq', 'pfcmpge', 'pfcmpgt', 'pfmax', 'pfmin', 'pfmul', 'pfrcp', 'pfrcpit1', 'pfrcpit2', 'pfrsqit1',
            'pfrsqrt', 'pfsub', 'pfsubr', 'pi2fd', 'pmachriw', 'pmagw', 'pmulhriw', 'pmulhrwa', 'pmulhrwc', 'pmvgezb', 'pmvlzb', 'pmvnzb', 'pmvzb', 'popaw',
            'popfw', 'prefetch', 'prefetchw', 'psubsiw', 'pushaw', 'pushfw', 'rdshr', 'retw', 'retfw', 'retnw', 'retd', 'retfd', 'retnd', 'rsdc', 'rsldt',
            'salc', 'segcs', 'segds', 'seges', 'segfs', 'seggs', 'segss', 'smi', 'smint', 'smintold', 'svdc', 'svldt', 'svts', 'ud1', 'umov', 'wrshr',
            'xbts', 'xlatb', 'xstore', 'xcryptecb', 'xcryptcbc', 'xcryptcfb', 'xcryptofb', 'cmov', 'j', 'set', 'movs', 'cmps', 'scas', 'lods', 'stos',
            'cmpeqps', 'cmpeqss', 'cmpleps', 'cmpless', 'cmpltps', 'cmpltss', 'cmpneqps', 'cmpneqss', 'cmpnleps', 'cmpnless', 'cmpnltps', 'cmpnltss',
            'cmpordps', 'cmpordss', 'cmpunordps', 'cmpunordss', 'ldmxcsr', 'stmxcsr', 'fxrstor', 'fxsave', 'maskmovq', 'movntq', 'pshufw', 'pfnacc',
            'pfpnacc', 'pi2fw', 'pf2iw', 'pswapd', 'ffreep', 'clflush', 'pause', 'movdq2q', 'movq2dq', 'cmpeqpd', 'cmpeqsd', 'cmplepd', 'cmplesd',
            'cmpltpd', 'cmpltsd', 'cmpneqpd', 'cmpneqsd', 'cmpnlepd', 'cmpnlesd', 'cmpnltpd', 'cmpnltsd', 'cmpordpd', 'cmpordsd', 'cmpunordpd',
            'cmpunordsd', 'vmread', 'vmwrite', 'vmcall', 'vmlaunch', 'vmresume', 'vmxoff', 'vmxon', 'vmclear', 'vmptrld', 'vmptrst', 'vmrun', 'vmmcall',
            'vmload', 'vmsave', 'stgi', 'clgi', 'skinit', 'invlpga', 'montmul', 'xsha1', 'xsha256', 'dmint', 'rdm', 'movntss', 'movntsd', 'insertq',
            'extrq', 'pcmpestri', 'pcmpestrm', 'pcmpistri', 'pcmpistrm', 'rdtscp', 'movbe', 'vaddpd', 'vaddps', 'vaddsd', 'vaddss', 'vaddsubpd', 'vaddsubps',
            'vaesdec', 'vaesdeclast', 'vaesenc', 'vaesenclast', 'vaesimc', 'vaeskeygenassist', 'vandnpd', 'vandnps', 'vandpd', 'vandps', 'vblendpd', 'vblendps',
            'vblendvpd', 'vblendvps', 'vbroadcastf128', 'vbroadcastsd', 'vbroadcastss', 'vcmpeqps', 'vcmpltps', 'vcmpleps', 'vcmpunordps', 'vcmpneqps',
            'vcmpnltps', 'vcmpnleps', 'vcmpordps', 'vcmpeq_uqps', 'vcmpngeps', 'vcmpngtps', 'vcmpfalseps', 'vcmpneq_oqps', 'vcmpgeps', 'vcmpgtps', 'vcmptrueps',
            'vcmpeq_osps', 'vcmplt_oqps', 'vcmple_oqps', 'vcmpunord_sps', 'vcmpneq_usps', 'vcmpnlt_uqps', 'vcmpnle_uqps', 'vcmpord_sps', 'vcmpeq_usps',
            'vcmpnge_uqps', 'vcmpngt_uqps', 'vcmpfalse_osps', 'vcmpneq_osps', 'vcmpge_oqps', 'vcmpgt_oqps', 'vcmptrue_usps', 'vcmpeqpd', 'vcmpltpd', 'vcmplepd',
            'vcmpunordpd', 'vcmpneqpd', 'vcmpnltpd', 'vcmpnlepd', 'vcmpordpd', 'vcmpeq_uqpd', 'vcmpngepd', 'vcmpngtpd', 'vcmpfalsepd', 'vcmpneq_oqpd',
            'vcmpgepd', 'vcmpgtpd', 'vcmptruepd', 'vcmpeq_ospd', 'vcmplt_oqpd', 'vcmple_oqpd', 'vcmpunord_spd', 'vcmpneq_uspd', 'vcmpnlt_uqpd', 'vcmpnle_uqpd',
            'vcmpord_spd', 'vcmpeq_uspd', 'vcmpnge_uqpd', 'vcmpngt_uqpd', 'vcmpfalse_ospd', 'vcmpneq_ospd', 'vcmpge_oqpd', 'vcmpgt_oqpd', 'vcmptrue_uspd',
            'vcmppd', 'vcmpps', 'vcmpsd', 'vcmpss', 'vcomisd', 'vcomiss', 'vcvtdq2pd', 'vcvtdq2ps', 'vcvtpd2dq', 'vcvtpd2ps', 'vcvtph2ps', 'vcvtps2dq',
            'vcvtps2pd', 'vcvtps2ph', 'vcvtsd2si', 'vcvtsd2ss', 'vcvtsi2sd', 'vcvtss2si', 'vcvttpd2dq', 'vcvttps2dq', 'vcvttsd2si', 'vcvttss2si', 'vdivpd',
            'vdivps', 'vdivsd', 'vdivss', 'vdppd', 'vdpps', 'vextractf128', 'vextractps', 'vhaddpd', 'vhaddps', 'vhsubpd', 'vhsubps', 'vinsertf128', 'vinsertps',
            'vlddqu', 'vldmxcsr', 'vmaskmovdqu', 'vmaskmovpd', 'vmaskmovps', 'vmaxpd', 'vmaxps', 'vmaxsd', 'vmaxss', 'vminpd', 'vminps', 'vminsd', 'vminss',
            'vmovapd', 'vmovaps', 'vmovd', 'vmovddup', 'vmovdqa', 'vmovhlps', 'vmovhpd', 'vmovhps', 'vmovlhps', 'vmovlpd', 'vmovlps', 'vmovmskpd', 'vmovmskps',
            'vmovntdq', 'vmovntdqa', 'vmovntpd', 'vmovntps', 'vmovq', 'vmovshdup', 'vmovsldup', 'vmovupd', 'vmovups', 'vmpsadbw', 'vmulpd', 'vmulps', 'vmulsd',
            'vorpd', 'vorps', 'vpabsb', 'vpabsd', 'vpabsw', 'vpackssdw', 'vpacksswb', 'vpackusdw', 'vpackuswb', 'vpaddb', 'vpaddd', 'vpaddq', 'vpaddsb',
            'vpaddsw', 'vpaddusb', 'vpaddusw', 'vpaddw', 'vpalignr', 'vpand', 'vpandn', 'vpavgb', 'vpavgw', 'vpblendvb', 'vpblendw', 'vpclmulqdq', 'vpcmpeqb',
            'vpcmpeqd', 'vpcmpeqq', 'vpcmpeqw', 'vpcmpestri', 'vpcmpestrm', 'vpcmpgtb', 'vpcmpgtd', 'vpcmpgtq', 'vpcmpgtw', 'vpcmpistri', 'vpcmpistrm',
            'vperm2f128', 'vpermilpd', 'vpermilps', 'vpextrb', 'vpextrd', 'vpextrq', 'vpextrw', 'vphaddd', 'vphaddsw', 'vphaddw', 'vphminposuw', 'vphsubd',
            'vphsubsw', 'vphsubw', 'vpinsrb', 'vpinsrd', 'vpinsrq', 'vpinsrw', 'vpmaddubsw', 'vpmaddwd', 'vpmaxsb', 'vpmaxsd', 'vpmaxsw', 'vpmaxub', 'vpmaxud',
            'vpmaxuw', 'vpminsb', 'vpminsd', 'vpminsw', 'vpminub', 'vpminud', 'vpminuw', 'vpmovmskb', 'vpmovsxbd', 'vpmovsxbq', 'vpmovsxbw', 'vpmovsxdq',
            'vpmovsxwd', 'vpmovsxwq', 'vpmovzxbd', 'vpmovzxbq', 'vpmovzxbw', 'vpmovzxdq', 'vpmovzxwd', 'vpmovzxwq', 'vpmuldq', 'vpmulhrsw', 'vpmulhuw',
            'vpmulhw', 'vpmulld', 'vpmullw', 'vpmuludq', 'vpor', 'vpsadbw', 'vpshufb', 'vpshufd', 'vpshufhw', 'vpshuflw', 'vpsignb', 'vpsignd', 'vpsignw',
            'vpslld', 'vpslldq', 'vpsllq', 'vpsllw', 'vpsrad', 'vpsraw', 'vpsrld', 'vpsrldq', 'vpsrlq', 'vpsrlw', 'vpsubb', 'vpsubd', 'vpsubq', 'vpsubsb',
            'vpsubsw', 'vpsubusb', 'vpsubusw', 'vpsubw', 'vptest', 'vpunpckhbw', 'vpunpckhdq', 'vpunpckhqdq', 'vpunpckhwd', 'vpunpcklbw', 'vpunpckldq',
            'vpunpcklqdq', 'vpunpcklwd', 'vpxor', 'vrcpps', 'vrcpss', 'vroundpd', 'vroundps', 'vroundsd', 'vroundss', 'vrsqrtps', 'vrsqrtss', 'vshufpd',
            'vshufps', 'vsqrtpd', 'vsqrtps', 'vsqrtsd', 'vsqrtss', 'vstmxcsr', 'vsubpd', 'vsubps', 'vsubsd', 'vsubss', 'vtestpd', 'vtestps', 'vucomiss',
            'vunpckhpd', 'vunpckhps', 'vunpcklpd', 'vunpcklps', 'vxorpd', 'vzeroall', 'sarx', 'shlx', 'shrx', 'adcx', 'adox', 'vbroadcasti128', 'vextracti128',
            'vinserti128', 'vpblendd', 'vpbroadcastb', 'vpbroadcastd', 'vpbroadcastq', 'vpbroadcastw', 'vperm2i128', 'vpermd', 'vpermpd', 'vpermps', 'vpermq',
            'vpmaskmovd', 'vpmaskmovq', 'vpsllvd', 'vpsllvq', 'vpsravd', 'vpsrlvd', 'vpsrlvq', 'vgatherdpd', 'vgatherdps', 'vgatherqpd', 'vgatherqps', 'vpgatherdd',
            'vpgatherdq', 'vpgatherqd', 'vpgatherqq', 'vfmaddpd', 'vfmadd132pd', 'vfmadd132ps', 'vfmadd132sd', 'vfmadd132ss', 'vfmadd213pd', 'vfmadd213ps',
            'vfmadd213sd', 'vfmadd213ss', 'vfmadd231pd', 'vfmadd231ps', 'vfmadd231sd', 'vfmadd231ss', 'vfmaddsub132pd', 'vfmaddsub132ps', 'vfmaddsub213pd',
            'vfmaddsub213ps', 'vfmaddsub231pd', 'vfmaddsub231ps', 'vfmsub132pd', 'vfmsub132ps', 'vfmsub132sd', 'vfmsub132ss', 'vfmsub213pd', 'vfmsub213ps',
            'vfmsub213sd', 'vfmsub213ss', 'vfmsub231pd', 'vfmsub231ps', 'vfmsub231sd', 'vfmsub231ss', 'vfmsubadd132pd', 'vfmsubadd132ps', 'vfmsubadd213pd',
            'vfmsubadd213ps', 'vfmsubadd231pd', 'vfmsubadd231ps', 'vfnmadd132pd', 'vfnmadd132ps', 'vfnmadd132sd', 'vfnmadd132ss', 'vfnmadd213pd', 'vfnmadd213ps',
            'vfnmadd213sd', 'vfnmadd213ss', 'vfnmadd231pd', 'vfnmadd231ps', 'vfnmadd231sd', 'vfnmadd231ss', 'vfnmsub132pd', 'vfnmsub132ps', 'vfnmsub132sd',
            'vfnmsub132ss', 'vfnmsub213pd', 'vfnmsub213ps', 'vfnmsub213sd', 'vfnmsub213ss', 'vfnmsub231pd', 'vfnmsub231ps', 'vfnmsub231sd', 'vfnmsub231ss',
            'xacquire', 'xrelease', 'xbegin', 'xabort', 'xend', 'xtest', 'rdrand', 'rdseed', 'xgetbv', 'prefetchwt1', 'kaddb', 'kaddd', 'kaddq', 'kaddw',
            'kandb', 'kandd', 'kandnb', 'kandnd', 'kandnq', 'kandnw', 'kandq', 'kandw', 'kmovb', 'kmovd', 'kmovq', 'kmovw', 'knotb', 'knotd', 'knotq', 'knotw',
            'korb', 'kord', 'korq', 'kortestb', 'kortestd', 'kortestq', 'kortestw', 'korw', 'kshiftlb', 'kshiftld', 'kshiftlq', 'kshiftlw', 'kshiftrb',
            'kshiftrd', 'kshiftrq', 'kshiftrw', 'ktestb', 'ktestd', 'ktestq', 'ktestw', 'kunpckbw', 'kunpckdq', 'kunpckwd', 'kxnorb', 'kxnord', 'kxnorq',
            'kxnorw', 'kxorb', 'kxord', 'kxorq', 'kxorw', 'valignd', 'valignq', 'vblendmpd', 'vblendmps', 'vbroadcastf32x2', 'vbroadcastf32x4', 'vbroadcastf32x8',
            'vbroadcastf64x2', 'vbroadcastf64x4', 'vbroadcasti32x2', 'vbroadcasti32x4', 'vbroadcasti32x8', 'vbroadcasti64x2', 'vbroadcasti64x4', 'vcompresspd',
            'vcompressps', 'vcvtpd2qq', 'vcvtpd2udq', 'vcvtpd2uqq', 'vcvtps2qq', 'vcvtps2udq', 'vcvtps2uqq', 'vcvtqq2pd', 'vcvtqq2ps', 'vcvtsd2usi', 'vcvtss2usi',
            'vcvttpd2qq', 'vcvttpd2udq', 'vcvttpd2uqq', 'vcvttps2qq', 'vcvttps2udq', 'vcvttps2uqq', 'vcvttsd2usi', 'vcvttss2usi', 'vcvtudq2pd', 'vcvtudq2ps',
            'vcvtuqq2pd', 'vcvtuqq2ps', 'vcvtusi2sd', 'vcvtusi2ss', 'vdbpsadbw', 'vexp2pd', 'vexp2ps', 'vexpandpd', 'vexpandps', 'vextractf32x4', 'vextractf32x8',
            'vextractf64x2', 'vextractf64x4', 'vextracti32x4', 'vextracti32x8', 'vextracti64x2', 'vextracti64x4', 'vfixupimmpd', 'vfixupimmps', 'vfixupimmsd',
            'vfixupimmss', 'vfpclasspd', 'vfpclassps', 'vfpclasssd', 'vfpclassss', 'vgetexppd', 'vgetexpps', 'vgetexpsd', 'vgetexpss', 'vgetmantpd', 'vgetmantps',
            'vgetmantsd', 'vgetmantss', 'vinsertf32x4', 'vinsertf32x8', 'vinsertf64x2', 'vinsertf64x4', 'vinserti32x4', 'vinserti32x8', 'vinserti64x2',
            'vinserti64x4', 'vmovdqa32', 'vmovdqa64', 'vmovdqu64', 'vpabsq', 'vpandd', 'vpandnd', 'vpandnq', 'vpandq', 'vpblendmb', 'vpblendmd', 'vpblendmq',
            'vpblendmw', 'vpcmpb', 'vpcmpd', 'vpcmpq', 'vpcmpub', 'vpcmpud', 'vpcmpuq', 'vpcmpuw', 'vpcmpw', 'vpcompressd', 'vpcompressq', 'vpconflictd',
            'vpconflictq', 'vpermb', 'vpermi2b', 'vpermi2d', 'vpermi2pd', 'vpermi2ps', 'vpermi2q', 'vpermi2w', 'vpermt2b', 'vpermt2d', 'vpermt2pd', 'vpermt2ps',
            'vpermt2q', 'vpermt2w', 'vpermw', 'vpexpandd', 'vpexpandq', 'vplzcntd', 'vplzcntq', 'vpmadd52huq', 'vpmadd52luq', 'vpmaxsq', 'vpmaxuq', 'vpminsq',
            'vpminuq', 'vpmovb2m', 'vpmovd2m', 'vpmovdb', 'vpmovdw', 'vpmovm2b', 'vpmovm2d', 'vpmovm2q', 'vpmovm2w', 'vpmovq2m', 'vpmovqb', 'vpmovqd', 'vpmovqw',
            'vpmovsdb', 'vpmovsdw', 'vpmovsqb', 'vpmovsqd', 'vpmovsqw', 'vpmovswb', 'vpmovusdb', 'vpmovusdw', 'vpmovusqb', 'vpmovusqd', 'vpmovusqw', 'vpmovuswb',
            'vpmovw2m', 'vpmovwb', 'vpmullq', 'vpmultishiftqb', 'vpord', 'vporq', 'vprold', 'vprolq', 'vprolvd', 'vprolvq', 'vprord', 'vprorq', 'vprorvd', 'vprorvq',
            'vpscatterdd', 'vpscatterdq', 'vpscatterqd', 'vpscatterqq', 'vpsllvw', 'vpsraq', 'vpsravq', 'vpsravw', 'vpsrlvw', 'vpternlogd', 'vpternlogq', 'vptestmb',
            'vptestmd', 'vptestmq', 'vptestmw', 'vptestnmb', 'vptestnmd', 'vptestnmq', 'vptestnmw', 'vpxord', 'vpxorq', 'vrangepd', 'vrangeps', 'vrangesd',
            'vrangess', 'vrcp14pd', 'vrcp14ps', 'vrcp14sd', 'vrcp14ss', 'vrcp28pd', 'vrcp28ps', 'vrcp28sd', 'vrcp28ss', 'vreducepd', 'vreduceps', 'vreducesd',
            'vreducess', 'vrndscalepd', 'vrndscaleps', 'vrndscalesd', 'vrndscaless', 'vrsqrt14pd', 'vrsqrt14ps', 'vrsqrt14sd', 'vrsqrt14ss', 'vrsqrt28pd',
            'vrsqrt28ps', 'vrsqrt28sd', 'vrsqrt28ss', 'vscalefpd', 'vscalefps', 'vscalefsd', 'vscalefss', 'vscatterdpd', 'vscatterdps', 'vscatterqpd',
            'vscatterqps', 'vshuff32x4', 'vshuff64x2', 'vshufi32x4', 'vshufi64x2', 'none', 'cbtw', 'cltd', 'cmpsl', 'cwtl', 'insl', 'lodsl', 'movsl', 'movz', 'outsl',
            'popal', 'popfl', 'pushal', 'pushfl', 'lret', 'lretw', 'scasl', 'cs', 'ds', 'es', 'fs', 'gs', 'ss', 'stosl', 'add4s', 'brkem', 'clr1',
            'cmp4s', 'ext', 'nec_ins', 'not1', 'repc', 'repnc', 'rol4', 'ror4', 'set1', 'sub4s', 'test1',
            'bb0_reset', 'bb1_reset', 'blcfill', 'blci', 'blcic', 'blcmsk', 'blcs', 'blsfill', 'blsic', 'bndcl', 'bndcn',
            'bndcu', 'bndldx', 'bndmk', 'bndmov', 'bndstx', 'cdqe', 'clac', 'clflushopt', 'clwb', 'clzero', 'cmpsq',
            'cmpxchg16b', 'cpu_read', 'cpu_write', 'cqo', 'db', 'dd', 'do', 'dq', 'dt', 'dw', 'dy', 'dz', 'equ', 'fxrstor64',
            'fxsave64', 'getsec', 'hint_nop0', 'hint_nop1', 'hint_nop10', 'hint_nop11', 'hint_nop12', 'hint_nop13', 'hint_nop14',
            'hint_nop15', 'hint_nop16', 'hint_nop17', 'hint_nop18', 'hint_nop19', 'hint_nop2', 'hint_nop20', 'hint_nop21', 'hint_nop22',
            'hint_nop23', 'hint_nop24', 'hint_nop25', 'hint_nop26', 'hint_nop27', 'hint_nop28', 'hint_nop29', 'hint_nop3', 'hint_nop30',
            'hint_nop31', 'hint_nop32', 'hint_nop33', 'hint_nop34', 'hint_nop35', 'hint_nop36', 'hint_nop37', 'hint_nop38', 'hint_nop39',
            'hint_nop4', 'hint_nop40', 'hint_nop41', 'hint_nop42', 'hint_nop43', 'hint_nop44', 'hint_nop45', 'hint_nop46', 'hint_nop47',
            'hint_nop48', 'hint_nop49', 'hint_nop5', 'hint_nop50', 'hint_nop51', 'hint_nop52', 'hint_nop53', 'hint_nop54', 'hint_nop55',
            'hint_nop56', 'hint_nop57', 'hint_nop58', 'hint_nop59', 'hint_nop6', 'hint_nop60', 'hint_nop61', 'hint_nop62', 'hint_nop63',
            'hint_nop7', 'hint_nop8', 'hint_nop9', 'incbin', 'invept', 'invpcid', 'invvpid', 'iretq', 'jmpe', 'jrcxz', 'llwpcb', 'lodsq',
            'lwpins', 'lwpval', 'monitorx', 'mwaitx', 'pclmulhqhqdq', 'pclmulhqlqdq', 'pclmullqhqdq', 'pclmullqlqdq', 'pcommit', 'pfrcpv',
            'pfrsqrtv', 'popfq', 'pushfq', 'rdfsbase', 'rdgsbase', 'rdpid', 'rdpkru', 'resb', 'resd', 'reso', 'resq', 'rest', 'resw', 'resy',
            'resz', 'rsts', 'scasq', 'sha1msg1', 'sha1msg2', 'sha1nexte', 'sha1rnds4', 'sha256msg1', 'sha256msg2', 'sha256rnds2', 'slwpcb',
            'stac', 'swapgs', 't1mskc', 'tzmsk', 'ud0', 'ud2a', 'ud2b', 'vcmpeqsd', 'vcmpeqss', 'vcmpeq_ossd', 'vcmpeq_osss', 'vcmpeq_uqsd',
            'vcmpeq_uqss', 'vcmpeq_ussd', 'vcmpeq_usss', 'vcmpfalsesd', 'vcmpfalsess', 'vcmpfalse_oqpd', 'vcmpfalse_oqps', 'vcmpfalse_oqsd',
            'vcmpfalse_oqss', 'vcmpfalse_ossd', 'vcmpfalse_osss', 'vcmpgesd', 'vcmpgess', 'vcmpge_oqsd', 'vcmpge_oqss', 'vcmpge_ospd', 'vcmpge_osps',
            'vcmpge_ossd', 'vcmpge_osss', 'vcmpgtsd', 'vcmpgtss', 'vcmpgt_oqsd', 'vcmpgt_oqss', 'vcmpgt_ospd', 'vcmpgt_osps', 'vcmpgt_ossd',
            'vcmpgt_osss', 'vcmplesd', 'vcmpless', 'vcmple_oqsd', 'vcmple_oqss', 'vcmple_ospd', 'vcmple_osps', 'vcmple_ossd', 'vcmple_osss',
            'vcmpltsd', 'vcmpltss', 'vcmplt_oqsd', 'vcmplt_oqss', 'vcmplt_ospd', 'vcmplt_osps', 'vcmplt_ossd', 'vcmplt_osss', 'vcmpneqsd',
            'vcmpneqss', 'vcmpneq_oqsd', 'vcmpneq_oqss', 'vcmpneq_ossd', 'vcmpneq_osss', 'vcmpneq_uqpd', 'vcmpneq_uqps', 'vcmpneq_uqsd',
            'vcmpneq_uqss', 'vcmpneq_ussd', 'vcmpneq_usss', 'vcmpngesd', 'vcmpngess', 'vcmpnge_uqsd', 'vcmpnge_uqss', 'vcmpnge_uspd',
            'vcmpnge_usps', 'vcmpnge_ussd', 'vcmpnge_usss', 'vcmpngtsd', 'vcmpngtss', 'vcmpngt_uqsd', 'vcmpngt_uqss', 'vcmpngt_uspd',
            'vcmpngt_usps', 'vcmpngt_ussd', 'vcmpngt_usss', 'vcmpnlesd', 'vcmpnless', 'vcmpnle_uqsd', 'vcmpnle_uqss', 'vcmpnle_uspd',
            'vcmpnle_usps', 'vcmpnle_ussd', 'vcmpnle_usss', 'vcmpnltsd', 'vcmpnltss', 'vcmpnlt_uqsd', 'vcmpnlt_uqss', 'vcmpnlt_uspd',
            'vcmpnlt_usps', 'vcmpnlt_ussd', 'vcmpnlt_usss', 'vcmpordsd', 'vcmpordss', 'vcmpord_qpd', 'vcmpord_qps', 'vcmpord_qsd', 'vcmpord_qss',
            'vcmpord_ssd', 'vcmpord_sss', 'vcmptruesd', 'vcmptruess', 'vcmptrue_uqpd', 'vcmptrue_uqps', 'vcmptrue_uqsd', 'vcmptrue_uqss',
            'vcmptrue_ussd', 'vcmptrue_usss', 'vcmpunordsd', 'vcmpunordss', 'vcmpunord_qpd', 'vcmpunord_qps', 'vcmpunord_qsd', 'vcmpunord_qss',
            'vcmpunord_ssd', 'vcmpunord_sss', 'vfmadd123pd', 'vfmadd123ps', 'vfmadd123sd', 'vfmadd123ss', 'vfmadd312pd', 'vfmadd312ps',
            'vfmadd312sd', 'vfmadd312ss', 'vfmadd321pd', 'vfmadd321ps', 'vfmadd321sd', 'vfmadd321ss', 'vfmaddps', 'vfmaddsd', 'vfmaddss',
            'vfmaddsub123pd', 'vfmaddsub123ps', 'vfmaddsub312pd', 'vfmaddsub312ps', 'vfmaddsub321pd', 'vfmaddsub321ps', 'vfmaddsubpd',
            'vfmaddsubps', 'vfmsub123pd', 'vfmsub123ps', 'vfmsub123sd', 'vfmsub123ss', 'vfmsub312pd', 'vfmsub312ps', 'vfmsub312sd', 'vfmsub312ss',
            'vfmsub321pd', 'vfmsub321ps', 'vfmsub321sd', 'vfmsub321ss', 'vfmsubadd123pd', 'vfmsubadd123ps', 'vfmsubadd312pd', 'vfmsubadd312ps',
            'vfmsubadd321pd', 'vfmsubadd321ps', 'vfmsubaddpd', 'vfmsubaddps', 'vfmsubpd', 'vfmsubps', 'vfmsubsd', 'vfmsubss', 'vfnmadd123pd',
            'vfnmadd123ps', 'vfnmadd123sd', 'vfnmadd123ss', 'vfnmadd312pd', 'vfnmadd312ps', 'vfnmadd312sd', 'vfnmadd312ss', 'vfnmadd321pd',
            'vfnmadd321ps', 'vfnmadd321sd', 'vfnmadd321ss', 'vfnmaddpd', 'vfnmaddps', 'vfnmaddsd', 'vfnmaddss', 'vfnmsub123pd', 'vfnmsub123ps',
            'vfnmsub123sd', 'vfnmsub123ss', 'vfnmsub312pd', 'vfnmsub312ps', 'vfnmsub312sd', 'vfnmsub312ss', 'vfnmsub321pd', 'vfnmsub321ps',
            'vfnmsub321sd', 'vfnmsub321ss', 'vfnmsubpd', 'vfnmsubps', 'vfnmsubsd', 'vfnmsubss', 'vfrczpd', 'vfrczps', 'vfrczsd', 'vfrczss',
            'vgatherpf0dpd', 'vgatherpf0dps', 'vgatherpf0qpd', 'vgatherpf0qps', 'vgatherpf1dpd', 'vgatherpf1dps', 'vgatherpf1qpd', 'vgatherpf1qps',
            'vldqqu', 'vmfunc', 'vmovntqq', 'vmovqqa', 'vmovqqu', 'vpbroadcastmb2q', 'vpbroadcastmw2d', 'vpclmulhqhqdq', 'vpclmulhqlqdq',
            'vpclmullqhqdq', 'vpclmullqlqdq', 'vpcmov', 'vpcomb', 'vpcomd', 'vpcomq', 'vpcomub', 'vpcomud', 'vpcomuq', 'vpcomuw', 'vpcomw',
            'vphaddbd', 'vphaddbq', 'vphaddbw', 'vphadddq', 'vphaddubd', 'vphaddubq', 'vphaddubw', 'vphaddudq', 'vphadduwd', 'vphadduwq',
            'vphaddwd', 'vphaddwq', 'vphsubbw', 'vphsubdq', 'vphsubwd', 'vpmacsdd', 'vpmacsdqh', 'vpmacsdql', 'vpmacssdd', 'vpmacssdqh',
            'vpmacssdql', 'vpmacsswd', 'vpmacssww', 'vpmacswd', 'vpmacsww', 'vpmadcsswd', 'vpmadcswd', 'vpperm', 'vprotb', 'vprotd', 'vprotq',
            'vprotw', 'vpshab', 'vpshad', 'vpshaq', 'vpshaw', 'vpshlb', 'vpshld', 'vpshlq', 'vpshlw', 'vscatterpf0dpd', 'vscatterpf0dps',
            'vscatterpf0qpd', 'vscatterpf0qps', 'vscatterpf1dpd', 'vscatterpf1dps', 'vscatterpf1qpd', 'vscatterpf1qps', 'wrfsbase', 'wrgsbase',
            'wrpkru', 'xcryptctr', 'xrstor', 'xrstor64', 'xrstors', 'xrstors64', 'xsave', 'xsave64', 'xsavec', 'xsavec64', 'xsaveopt', 'xsaveopt64',
            'xsaves', 'xsaves64', 'xsetbv',
            /* spellchecker: enable */
        ].join('|') + ')(?:$|\\s)')
    };

    function conditional(mnemonic: string) {
        /* spellchecker: disable */
        return mnemonic + '(?:n?[abgl]e?|n?[ceosz]|np|p[eo]?)';
        /* spellchecker: enable */
    }

    return {
        startState() {
            return {};
        },

        token(stream) {
            if (stream.eatSpace()) {
                return null;
            }

            if (stream.eat(';')) {
                stream.skipToEnd();
                return 'comment';
            }

            // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
            if (stream.match(/\w+:/)) {
                return 'tag';
            }

            for (const key in grammar) {
                // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
                if (stream.match(grammar[key as keyof typeof grammar])) {
                    return key;
                }
            }

            stream.match(/\S+/);
            return null;
        }
    };
});

CodeMirror.defineMIME('text/x-asm', 'asm');