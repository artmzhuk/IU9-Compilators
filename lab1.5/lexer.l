%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define TAG_IDENT 1
#define TAG_NUMBER 2
#define TAG_STRING 3

char* tag_names[] = {
    "END_OF_PROGRAM", "IDENT", "NUMBER",
    "STRING"
};

typedef struct {
    int line, pos, index;
} Position;

typedef struct {
    Position starting, following;
} Fragment;

typedef struct {
    int type;
    Fragment coords;
    union {
        long long number_value;
        char* string_value;
    } attr;
} Token;

typedef struct {
    Position pos;
    char* message;
} Error;

typedef struct {
    Token* items;
    int count;
    int capacity;
} TokenArray;



typedef struct {
    Error* items;
    int count;
    int capacity;
} ErrorArray;

typedef struct {
    char** items;
    int count;
    int capacity;
} IdentTable;

TokenArray tokens = {NULL, 0, 0};
ErrorArray errors = {NULL, 0, 0};
IdentTable ident_table = {NULL, 0, 0};

char* string_buf = NULL;
size_t string_buf_len = 0;
size_t string_buf_cap = 0;

Position cur_pos = {1, 1, 0};
Position str_start;

void initialize(void** arr, int* count, int* capacity, size_t item_size, int initial_capacity) {
    *arr = malloc(item_size * initial_capacity);
    *count = 0;
    *capacity = initial_capacity;
}

void push_to(void** arr, int* count, int* capacity, size_t item_size, void* item) {
    if (*count >= *capacity) {
        *capacity *= 2;
        *arr = realloc(*arr, item_size * *capacity);
    }
    memcpy((char*)*arr + (*count * item_size), item, item_size);
    (*count)++;
}

int find_or_add_ident(const char* name) {
    for (int i = 0; i < ident_table.count; i++) {
        if (strcmp(ident_table.items[i], name) == 0) {
            return i + 1;
        }
    }

    if (ident_table.count >= ident_table.capacity) {
        ident_table.capacity = ident_table.capacity ? ident_table.capacity * 2 : 16;
        ident_table.items = realloc(ident_table.items, sizeof(char*) * ident_table.capacity);
    }

    ident_table.items[ident_table.count] = strdup(name);
    return ++ident_table.count;
}

void add_token(const char* type_name, Fragment frag, long long num_val, char* str_val) {
    Token token = {0};
    token.coords = frag;

    if (strcmp(type_name, "IDENT") == 0) {
        token.type = TAG_IDENT;
        token.attr.number_value = find_or_add_ident(str_val);
        free(str_val);
    }
    else if (strcmp(type_name, "NUMBER") == 0) {
        token.type = TAG_NUMBER;
        token.attr.number_value = num_val;
    }
    else if (strcmp(type_name, "STRING") == 0) {
        token.type = TAG_STRING;
        token.attr.string_value = strdup(str_val);
    }


    push_to((void**)&tokens.items, &tokens.count, &tokens.capacity, sizeof(Token), &token);
}


void push_error(Position pos, char* message) {
    Error error = { .pos = pos, .message = strdup(message) };
    push_to((void**)&errors.items, &errors.count, &errors.capacity, sizeof(Error), &error);
}

void print_frag(Fragment f) {
    printf("(%d,%d)-(%d,%d)", f.starting.line, f.starting.pos, f.following.line, f.following.pos);
}

void print_all() {
    printf("\nIDENTS (%d):\n", ident_table.count);
    for (int i = 0; i < ident_table.count; i++) {
        printf("%d: %s\n", i+1, ident_table.items[i]);
    }

    printf("\nTOKENS (%d):\n", tokens.count);
    for (int i = 0; i < tokens.count; i++) {
        print_frag(tokens.items[i].coords);
        printf(" %s", tag_names[tokens.items[i].type]);

        switch(tokens.items[i].type) {
            case TAG_IDENT:
                printf(" %lld (%s)\n", tokens.items[i].attr.number_value,
                       ident_table.items[tokens.items[i].attr.number_value - 1]);
                break;
            case TAG_STRING:
                printf(" %s\n", tokens.items[i].attr.string_value);
                break;
            case TAG_NUMBER:
                printf(" %lld\n", tokens.items[i].attr.number_value);
                break;
            default:
                printf("\n");
                break;
        }
    }


    printf("\nERRORS (%d):\n", errors.count);
    for (int i = 0; i < errors.count; i++) {
        printf("Error (%d,%d): %s\n", errors.items[i].pos.line, errors.items[i].pos.pos, errors.items[i].message);
    }
}

void free_arrays() {
    for (int i = 0; i < tokens.count; i++) {
        if (tokens.items[i].type == TAG_STRING) {
            free(tokens.items[i].attr.string_value);
        }
    }
    free(tokens.items);

    for (int i = 0; i < ident_table.count; i++) {
        free(ident_table.items[i]);
    }
    free(ident_table.items);


    for (int i = 0; i < errors.count; i++) {
        free(errors.items[i].message);
    }
    free(errors.items);

    if (string_buf) free(string_buf);
}

void update_position(const char* text, int len) {
    for (int i = 0; i < len; i++) {
        if (text[i] == '\n') {
            cur_pos.line++;
            cur_pos.pos = 1;
        } else {
            cur_pos.pos++;
        }
        cur_pos.index++;
    }
}

void clear_string_buf() {
    if (string_buf) free(string_buf);
    string_buf = NULL;
    string_buf_len = 0;
    string_buf_cap = 0;
}

void append_to_string(char c) {
    if (string_buf_len >= string_buf_cap) {
        string_buf_cap = string_buf_cap ? string_buf_cap * 2 : 16;
        string_buf = realloc(string_buf, string_buf_cap);
    }
    string_buf[string_buf_len++] = c;
}
%}

%option noyywrap nounput noinput

LETTER      [a-zA-Z_$@]
DIGIT       [0-9]
IDENT       {LETTER}({LETTER}|{DIGIT})*
NUMBER      {DIGIT}({DIGIT}|_)*
WS          [ \t]+

%x STRING ESCAPE

%%

{WS}        { update_position(yytext, yyleng); }

\"          {
    str_start = cur_pos;
    clear_string_buf();
    BEGIN(STRING);
    update_position(yytext, yyleng);
}

<STRING>{
  \"        {
    Fragment frag = {str_start, cur_pos};
    frag.following.pos++;
    frag.following.index++;
    append_to_string('\0');
    add_token("STRING", frag, 0, string_buf);
    clear_string_buf();
    update_position(yytext, yyleng);
    BEGIN(INITIAL);
  }
  \\n       { append_to_string('\n'); update_position(yytext, yyleng); }
  \\t       { append_to_string('\t'); update_position(yytext, yyleng); }
  \\\"      { append_to_string('"'); update_position(yytext, yyleng); }
  \\\\      { append_to_string('\\'); update_position(yytext, yyleng); }
  \\[^\n]   { push_error(cur_pos, "Invalid escape sequence"); update_position(yytext, yyleng); }
  \n        { push_error(cur_pos, "Newline in string"); update_position(yytext, yyleng); BEGIN(INITIAL); }
  .         { append_to_string(yytext[0]); update_position(yytext, yyleng); }
  <<EOF>>   { push_error(str_start, "Unclosed string"); clear_string_buf(); BEGIN(INITIAL); return 0; }
}

{NUMBER}    {
    Fragment frag = {cur_pos, cur_pos};
    frag.following.pos += yyleng;
    frag.following.index += yyleng;

    char* num_str = strdup(yytext);
    size_t j = 0;
    for (size_t i = 0; num_str[i]; i++) {
        if (num_str[i] != '_') num_str[j++] = num_str[i];
    }
    num_str[j] = '\0';

    char* end;
    long long num = strtoll(num_str, &end, 10);
    if (*end || errno == ERANGE) {
        push_error(cur_pos, "Invalid number");
    } else {
        add_token("NUMBER", frag, num, NULL);
    }
    free(num_str);
    update_position(yytext, yyleng);
}

{IDENT}     {
    Fragment frag = {cur_pos, cur_pos};
    frag.following.pos += yyleng;
    frag.following.index += yyleng;
    add_token("IDENT", frag, 0, strdup(yytext));
    update_position(yytext, yyleng);
}

\n          { cur_pos.line++; cur_pos.pos = 1; cur_pos.index++; }

.           { push_error(cur_pos, "Invalid character"); cur_pos.pos++; cur_pos.index++; }

%%

int main(int argc, char **argv) {
    initialize((void**)&tokens.items, &tokens.count, &tokens.capacity, sizeof(Token), 100);
    initialize((void**)&errors.items, &errors.count, &errors.capacity, sizeof(Error), 50);

    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            perror(argv[1]);
            return 1;
        }
    }

    yylex();
    print_all();
    free_arrays();

    if (yyin != stdin) fclose(yyin);
    return 0;
}