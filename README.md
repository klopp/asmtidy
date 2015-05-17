# asmtidy
Форматирование ассемблерных исходников

[Демо-страница](http://ato.su/asmtidy/).

```my $tidy = new asmtidy
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
    print $asmtidy->tidy_content( $asm_source );```

##Параметры

###indent_left
Число, отступ в пробелах от левого края:

```label:
        mov eax,ebx     ; 4
          xchg eax,ebx  ; 6```
          
###indent_comma
Число, отступы между запятой и вторым операндом, по умолчанию 0:
        
```mov eax,ebx     ; 0
    xchg eax, ebx   ; 1
    add eax,  ebx   ; 2```
    
###indent_tail_comment
Число, отступ между комментарием в хвосте строки и командами (выравнивается по самой длинной строке кода с комментарием):

```mov eax,ebx                ; 1
    lea edi,[esi+eax+12345678] ; 1
    
    mov eax,ebx                   ; 4
    lea edi,[esi+eax+12345678]    ; 4```
    
###indent_operands
Что делать после инструкции при наличии операндов. Возможные значения: **N** (число), вставлять N пробелов:

```lea eax,[edi]    ; 1
    lea    eax,[edi] ; 4```
    
Или **tabN** (**tab5** и т.д.), выравнивать по левому краю инструкции, вставляя необходимое количество пробелов до N. Если длина инструкции больше N — игнорировать.

```dec  ebx        ; tab5
    movzx esi,al    ; игнорируем

    dec    ebx        ; tab7
    movzx  esi,al     ; tab7
    punpckldq mm1,mm2 ; игнорируем```
    
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
