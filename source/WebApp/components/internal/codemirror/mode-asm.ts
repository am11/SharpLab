// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE
import CodeMirror from 'codemirror';

CodeMirror.defineMode('asm', () => {
    const grammar = {
        builtin: new RegExp('^(?:' + [
            /* spellchecker: disable */
            'aa[adms]', 'adc', 'add', 'and', 'arpl',
            'bound', 'bsf', 'bsr', 'bswap', 'bt', 'bt[crs]',
            'call', 'cbw', 'cdq', 'cl[cdi]', 'clts', 'cmc', conditional('cmov'), 'cmp', 'cmps[bdw]', 'cmpxchg', 'cmpxchg8b', 'cpuid', 'cwd', 'cwde',
            'daa', 'das', 'dec', 'div',
            'enter', 'esc',
            'hlt',
            'idiv', 'imul', 'in', 'inc', 'ins', 'insd', 'int', 'int[13o]', 'invd', 'invlpg', 'iret', 'iret[df]',
            conditional('j'), 'jecxz', 'jcxz', 'jmp',
            'lahf', 'lar', 'lds', 'lea', 'leave', 'les', 'l[gil]dt', 'lfs', 'lgs', 'lmsw', 'loadall', 'lock', 'lods[bdw]', 'loop', 'loop[dw]?', 'loopn?[ez][dw]?', 'lsl', 'lss', 'ltr',
            'mov', 'movs[bdw]', 'mov[sz]x', 'movsxd', 'mul',
            'neg', 'nop', 'not',
            'or', 'out', 'outsd?',
            'pop', 'pop[af]d?', 'push', 'push[af]d?',
            'rcl', 'rcr', 'rdmsr', 'rdpmc', 'rdtsc', 'rep', 'repn?[ez]', 'ret', 'retn', 'retf', 'rol', 'ror', 'rsm',
            'sahf', 'sal', 'sar', 'sbb', 'scas[bdw]', conditional('set'), 's[gil]dt', 'shld?', 'shrd?', 'smsw', 'st[cdi]', 'stos[bdw]', 'str', 'sub', 'syscall', 'sysenter', 'sysexit', 'sysret',
            'test',
            'ud2',
            'vcvtsi2ss', 'vcvtss2sd', 'verr', 'verw',
            'vmovdqu', 'vmovdqu8', 'vmovdqu16', 'vmovdqu32', 'vmovsd', 'vmovss', 'vmulss', 'vucomisd', 'vxorps', 'vzeroupper',
            'wait', 'wbinvd', 'wrmsr',
            'xadd', 'xchg', 'xlat', 'xor', 'xorps',

            //
            // Following ones are captured from x86 list, excluding the above:
            // https://github.com/dotnet/runtime/blob/ea35f6aae9e1aac0e0c5de6df97e05bd992a7528/src/coreclr/src/jit/instrsxarch.h
            //
            // Note: this is still not the exhaustive list, as it is missing intrinsic instructions.
            //
            'addpd', 'addps', 'addsd', 'addss', 'addsubpd', 'addsubps', 'aesdec', 'aesdeclast', 'aesenc', 'aesenclast',
            'aesimc', 'aeskeygenassist', 'align', 'andn', 'andnpd', 'andnps', 'andpd', 'andps', 'bextr', 'blendpd', 'blendps',
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
            'zeroupper'
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
