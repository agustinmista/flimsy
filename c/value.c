
// ----------------------------
// Literals
// ----------------------------

typedef int    Int;
typedef double Double; 
typedef char*  String;
typedef char   Bool;
typedef char   Char;

typedef enum {
  LITERAL_INT,
  LITERAL_DOUBLE,
  LITERAL_STRING,
  LITERAL_BOOL,
  LITERAL_CHAR
} Literal_tag;

typedef union { 
  Int    getInt;
  Double getDouble;
  String getString;
  Bool   getBool;
  Char   getChar
} Literal_data;

typedef struct {
  Literal_tag  tag;
  Literal_data data;
} Literal;

// ----------------------------
// Values
// ----------------------------

typedef enum { 
  VALUE_LITERAL,
  VALUE_CON,
  VALUE_TUP,
  VALUE_SUM,
  VALUE_NIL,
  VALUE_CONS,
  VALUE_IO,
  VALUE_CLOSURE,
  VALUE_THUNK
} Value_tag;
