# asmtidy
Форматирование ассемблерных исходников

[Демо-страница](http://ato.su/asmtidy/).

# asmtidy.pm

    my $tidy = new asmtidy
    (
      {
        'indent_left' => 4,
        'indent_comma' => 0,
        'indent_tail_comment' => 1,
        'indent_operands' => 1,
        'unaligned_comments' => 'right',
        'del_empty_lines' => 'yes',
        'user_names' => 'Invoke,register',
        'log_file' => '/tmp/asmtidy.log'
      }
    );
    print $asmtidy->tidy_file( 'test.asm' );
    print $asmtidy->tidy_content( $asm_source );


##Параметры

###indent_left
Число, отступ в пробелах от левого края:

    label:
        mov eax,ebx     ; 4
          xchg eax,ebx  ; 6
          
###indent_comma
Число, отступы между запятой и вторым операндом, по умолчанию 0:
        
    mov eax,ebx     ; 0
    xchg eax, ebx   ; 1
    add eax,  ebx   ; 2
    
###indent_tail_comment
Число, отступ между комментарием в хвосте строки и командами (выравнивается по самой длинной строке кода с комментарием):

    mov eax,ebx                ; 1
    lea edi,[esi+eax+12345678] ; 1
    
    mov eax,ebx                   ; 4
    lea edi,[esi+eax+12345678]    ; 4
    
###indent_operands
Что делать после инструкции при наличии операндов. Возможные значения: **N** (число), вставлять N пробелов:

    lea eax,[edi]    ; 1
    lea    eax,[edi] ; 4
    
Или **tabN** (**tab5** и т.д.), выравнивать по левому краю инструкции, вставляя необходимое количество пробелов до N. Если длина инструкции больше N — игнорировать.

    dec  ebx        ; tab5
    movzx esi,al    ; игнорируем

    dec    ebx        ; tab7
    movzx  esi,al     ; tab7
    punpckldq mm1,mm2 ; игнорируем
    
###unaligned_comments
Что делать с однострочными комментариями, которые не прижаты к левому краю:

* **left**: прижимать влево
* **right**: выравнивать вправо в соответствии с параметром **indent_left**
* **nothing**: оставлять как есть

###del_empty_lines
Удалять или нет пустые строки:

* **all**: удалять все
* **yes**: все, идущие подряд, кроме одной
* **no**: не удалять

###user_names
Пользовательские имена, которые не считать метками и не прижимать влево. Например, *Invoke*. Разделители - пробел, перевод строки, запятая, точка с запятой, регистр не учитывается. 

# asmtidy.pl

    Usage: $program {file|-} [options]. Valid options are:

      -bak extension
        Extension for backup file (default: orig)
    
      -del {yes|no}
        Delete empty lines (yes - leave only one, default: no)
    
      -io {N|tabN}
        Indent second operands; number of spaces (defaut, 1 space) or TAB's
        filled up to N spaces 
  
      -itc N
        Indent tail comments (default: 1 space)

      -lm N
        Left margin (default: 4 spaces)  
 
      -o file
        Output to file (default or -: stdout, +: rewrite source)
    
      -sc N
        Spaces between comma and second operand (default: 0)
    
      -slc {left|right}
        Move single line comments to left margin, or to next line begin 
        (default: leave unchanged)
    
      -un {name1[,name2;...]}
        List of user names (separators: , or ;)

    Examples:

      # read file.pl from stdin and output result to stdout:
      cat file.pl | $program -o - 
      # or
      cat file.pl | $program 

      # read file.pl and rewrite it by result:
      $program file.pl -o + 
      