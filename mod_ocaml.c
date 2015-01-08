/*
 *
 * $Id: mod_ocaml.c,v 0.01 2002/11/10 18:55:32 leo Exp $
 *
 * * * * * * * * * * * * * * * * * * * * * 
 * mod_OCAML	http://www.modocaml.org/ *
 * * * * * * * * * * * * * * * * * * * * *
 *
 *
 * Copyright (C) 2002	Andreas Brandmaier <mail@brandy-online.de>
 *			Leonhard Fellermayr <leo@slacky.de>
 *
 * 			All Rights Reserved
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of the Author nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"

typedef struct param {
  char **key, **value;		/* pairs of key and value */
  int count;			/* counts read parametres */
} param;

/* **** PART TYPES FOR ARGUMENT PARSER */

#define ARG_SEPARATOR		"&"
#define ARG_ASSOCIATOR		"="
#define VAR_PREFIX		"$"
#define VAR_UNDEF_VALUE		"*undef*"

/* **** INTERNAL ERROR #S **** */

#define ERR_FILE_WRITE		-2001
#define ERR_FILE_READ		-2002

/* **** PART TYPES FOR PARSER **** */

#define UNDEF			1000
#define IS_HTML_BLOCK		1001
#define IS_OCAML_BLOCK		1002

/* **** APACHE SERVER STRING **** */

#define MOD_OCAML_VERSION_INFO	"mod_ocaml/0.01"

/* **** PREDEFINED TAGS **** */

#define HTMLBLOCK_START		"Printf.printf(\""
#define HTMLBLOCK_END		"\");;"

#define OCMLBLOCK_START		"<?ocml"
#define OCMLBLOCK_END		"?>"

#define TMP_FILE_DIR		"/tmp"

#define PARSED_FILE_EXT		"ocml"
#define OUTPUT_FILE_EXT		"out"

#define HTML_ESCAPE_CHARS	"\""

/* **** GLOBAL VARS **** */

char	*glob_orig_code = NULL;			/* this has to be global */

/* **** SETTINGS TO BE OVERWRITTEN BY HTTPD.CONF DIRECTIVES **** */

char	*glob_interpreter_path = "/usr/local/bin/ocaml";

/* *************************************************************************
   ocml__strnocpy() - Extract string positions n..o from given string
   ************************************************************************* */

char *ocml__strnocpy (request_rec *r, char *str, int start, int end) {

  int	i;
  char	*new = (char *)ap_pcalloc(r->pool, sizeof (char *) * (end - start + 2));

  if (strlen(str) == 0) return str;			/* empty string	*/

  for (i = start; i <= end; i++)
    new[i-start] = str[i];

  return new;
  
} // ocml__strnocpy ()

/* *************************************************************************
   ocml__strpos () - find first occurance of pattern in theString and return
                     its start position to caller
   ************************************************************************* */

int ocml__strpos (char s1[], char s2[], int start) {

  int	i = 0, j;

  i = start;

  if ((strlen(s1) == 0) | (strlen(s2) == 0)) return -1;

  while (1) {

    j = 0;

    while (1) {
      if (s2[j] == '\0') return i;
      if (s1[i+j] != s2[j]) break;
      j++;
    } // end while

    if (s1[i] == '\0') return -1;
    i++;

  } // end while

} // ocml__strpos ()

/* *************************************************************************
   ocml__strcnt () - count occurances of pattern in string str && return count
   ************************************************************************* */

int ocml__strcnt (request_rec *r, char *str, char *pattern) {

  int	start = -1, count = 0, next = 0;

  while (next != -1) {

    next = ocml__strpos (str, pattern, start+1);
    start = next;
    if (next != -1) count++;

  } // end while

  return count;

}

/* *************************************************************************
   ocml__strshr () - shift string count characters to the right, starting
                     from position from
   ************************************************************************* */

/* generate string of length count with empty characters ("X"), and return   */
char *genEmpty (request_rec *r, int count) {

  int	i;
  char	*empty = (char *)ap_pcalloc(r->pool, sizeof (char *) * count);
  for (i = 0; i < count; i++)
    empty[i] = 'X';
  return empty;
}

char *ocml__strshr (request_rec *r, char *str, int from, int count) {

  int	i;
  char	*new;

  if (count < 1) return str;	/* => no right-shifting needed */

  if (from == 0)
    return ap_pstrcat (r->pool, genEmpty (r, count), str, NULL);
  else
    return ap_pstrcat (r->pool, ocml__strnocpy (r, str, 0, from-1), genEmpty (r, count), ocml__strnocpy (r, str, from, strlen(str) - 1), NULL);

} // ocml__strshr()

/* *************************************************************************
   ocml__sputonbegin ()- append src at beginning of dest string.
   ************************************************************************* */

char *ocml__sputonbegin (request_rec *r, char *dest, char *src) {

  return ap_pstrcat (r->pool, src, dest, NULL);		/* it's so easy	*/

} // ocml__sputonbegin ()

/* *************************************************************************
   ocml__sputonend ()- append src at end of dest string.
   ************************************************************************* */

char *ocml__sputonend (request_rec *r, char *dest, char *src) {

  return ap_pstrcat (r->pool, dest, src, NULL);		/* vice versa	*/

} // ocml__sputonend ()

/* *************************************************************************
   ocml__strremove () - remove count chars from string str, start at start
   ************************************************************************* */

char *ocml__strremove (request_rec *r, char *str, int start, int count) {

  if (start == 0)
    return ocml__strnocpy (r, str, start+count, strlen(str) - 1);
  else
    return ap_pstrcat (r->pool, ocml__strnocpy (r, str, 0, start - 1), ocml__strnocpy (r, str, start+count, strlen(str) - 1), NULL);

} // ocml__strremove ()


/* *************************************************************************
   ocml__rawurldecode () - Decode input from GET and POST requests and return
   ************************************************************************* */

char *ocml__rawurldecode (request_rec *r, char *input) {

  int	x, y;
  char	*target = (char *)ap_pcalloc(r->pool, sizeof (char *) * strlen (input));
  char	*hexstr = (char *)ap_pcalloc(r->pool, sizeof (char *) * strlen (input));

  if (strlen(input) == 0) return input;			/* empty string	*/

  for (x = 0, y = 0; x < strlen(input); x++, y++)

    switch (input[x]) {

      case '+':
        target[y] = ' ';
        break;

      case '%':
        hexstr = ap_pstrndup (r->pool, &input[x+1], 2);
        x += 2;
        if ( ((strcmp(hexstr,"26") == 0)) || ((strcmp(hexstr,"3D") == 0)) ) {
          target[y] = '%';
          y += 3;
          target = ap_pstrdup (r->pool, hexstr);
          break;
        } // end if
        target[y] = (char)strtol(hexstr, NULL, 16);
        break;
 
      default:
        target[y] = input[x];
        break;
 
    } // end switch

  return target;
 
} // ocml__rawurldecode ()

/* *************************************************************************
   ocml__get_val_for_key () - Get the value for given key from struct param *p
   ************************************************************************* */

char *ocml__get_val_for_key (request_rec *r, param *p, char *key) {

  int	i;

  if (strlen(key) == 0) return NULL;		/* empty key string	    */
  if (p->count == 0) return NULL;		/* no args available at all */
  
  for (i = 1; i <= p->count - 1; i++)
    if (strcmp(p->key[i], key) == 0)
      return p->value[i];

  return NULL;

} // ocml__get_val_for_key ()

/* *************************************************************************
   ocml__parse_cgi_args () - Parse the given string args for CGI-style params
   			     and don't trust any user input at all. We don't
   			     want to get stuck within our while()-loop.
   ************************************************************************* */

param *ocml__parse_cgi_args (request_rec *r, char *args) {

  param	*p;
  char	*this_arg;
  int	start = 0, end = 0, counter = 1;

  // allocate memory for ALL elements of struct
  p = (param *)ap_pcalloc(r->pool, sizeof (param *));
  p->key   = (char **)ap_pcalloc(r->pool, sizeof (char **) * 255);
  p->value = (char **)ap_pcalloc(r->pool, sizeof (char **) * 255);

  if (args == NULL) {				/* there ain't no args	    */
    p->count = 0;
    return p;
  } // end if
  
  this_arg = (char *) ap_pcalloc(r->pool, sizeof (char *));

  while ((end < strlen (args) - 1) && (start < strlen (args) - 1)) {

    /* calculate end of current arg by searching the next arg-separator	   */
    end = ocml__strpos (args, ARG_SEPARATOR, start + 1) - 1;

    /* special treatment for last argument (because no separator after it) */
    if (end < 0) end = strlen (args) - 1;

    /* if argument empty (consists of "&") -> skip it (=> increment start) */
    if (ocml__strpos (args, ARG_SEPARATOR, start) == start)

      start++;	/* now points to next character, let's go on. */

    /* if current argument has correct syntax (= is complete, &a=b) ...    */
    /* skip this for arguments without "=" and for arguments like "&a="    */
    
    if (

      /* if there is an associator value within argument (key=value) */
      (ocml__strpos (args, ARG_ASSOCIATOR, start + 1) != -1)

      /* ..and if associator argument isn't at end of string (= empty arg) */
      && (ocml__strpos (args, ARG_ASSOCIATOR, start + 1) < end)) { 

        this_arg = ocml__strnocpy (r, args, start, end);

        start += strlen (this_arg) + 1;

	/* check if there is exactly ONE associator character within arg   */
	/* if not, proceed at position start and go search the next arg    */

	if (ocml__strcnt(r, this_arg, ARG_ASSOCIATOR) == 1) {

          /* get key out from &bla=blubb */

          p->key[counter]   = ocml__strnocpy (r, this_arg, 0, ocml__strpos (this_arg, ARG_ASSOCIATOR, 0) - 1);
          p->value[counter] = ocml__rawurldecode (r, ocml__strnocpy (r, this_arg, ocml__strpos (this_arg, ARG_ASSOCIATOR, 0) + 1, strlen (this_arg) - 1));

          ++counter;

	} // end if (exactly one assoc in arg)

    } // end if (assoc is there && assoc is not at arg-end)

    else start++;	/* try it with following character		    */

  } // end while
  
  p->count = counter;	/* save our arg-counter for further use into struct */

  return p;

} // ocml__parse_cgi_args ()

/* *************************************************************************
   ocml__get_filesize () - determine filesize of a given (as string) file.
   ************************************************************************* */

long ocml__get_filesize (request_rec *r, char *file) {

  FILE	*stream;
  long	curpos, length;
  
  if ((stream = ap_pfopen (r->pool, file, "r")) == NULL)
    return ERR_FILE_READ;
  
  curpos = ftell (stream);
  fseek (stream, 0L, SEEK_END);
  length = ftell (stream);
  fseek (stream, curpos, SEEK_SET);

  ap_pfclose (r->pool, stream);
  
  return length;

} // ocml__get_filesize ()

/* *************************************************************************
   ocml__write_ocml () - write *code to *file.
   ************************************************************************* */

int ocml__write_ocml (request_rec *r, char *file, char *code) {

  FILE	*fp;

  if ((fp = ap_pfopen (r->pool, file, "w")) == NULL)
    return ERR_FILE_WRITE;
  fprintf (fp, "%s", code);
  ap_pfclose (r->pool, fp);

} // ocml__write_ocml ()

/* *************************************************************************
   ocml__read_ocml () - read a given file and return its content to caller.
   ************************************************************************* */

char *ocml__read_ocml (request_rec *r, char *file) {

  FILE	*fp;
  char	*code = (char *)ap_pcalloc(r->pool, sizeof (char *) * ocml__get_filesize (r, file));
  char	buf[255];

  if (ocml__get_filesize (r, file) == 0)
    return NULL;
    
  if ((fp = ap_pfopen (r->pool, file, "r")) == NULL)
    return NULL;

  while (!feof(fp)) {
    fgets (buf, 255, fp);
    if (!feof(fp))
      strcat (code, buf);
  } // end while

  ap_pfclose (r->pool, fp);

  return code;

} // ocml__read_ocml ()

/* *************************************************************************
   ocml__get_rand () - create random number within range and return this
   ************************************************************************* */

int ocml__get_rand (int range) {

  srand ((unsigned int)time((time_t *) NULL));

  return (int)((double)rand() / ((double)RAND_MAX + 1) * range);

} // ocml__get_rand ()

/* *************************************************************************
   ocml__generate_random_filename () - generate a random (~ unique) file name.
   ************************************************************************* */

char *ocml__generate_random_filename (request_rec *r, char *tmp_dir, char *fileext) {

  return ap_psprintf (r->pool, "%s/%d-%04d.%s", tmp_dir, time(NULL), ocml__get_rand (4095), fileext);
  
} // ocml__generate_random_filename ()

/* *************************************************************************
   ocml__escape_chars () - escape characters within code
   ************************************************************************* */

char *ocml__escape_chars (request_rec *r, char *code_esc, char *escape) {

  int	i, j;

  if ((strlen(escape) == 0) | (strlen(code_esc) == 0)) return code_esc;

  for (i = 0; i < strlen (escape); i++)
    for (j = 0; j < strlen (code_esc) - 1; j++)
    
      if (code_esc[j] == escape[i]) {
        code_esc = ocml__strshr (r, code_esc, j+1, 1);
        code_esc[j+1] = code_esc[j];
        code_esc[j] = '\\';
        j++;
      } // end if

  return code_esc;

} // ocml__escape_chars ()

/* *************************************************************************
   ocml__evaluate_vars () - Evaluate perl-style vars within OCAML embedded
   			    code
   ************************************************************************* */

char *ocml__evaluate_vars (request_rec *r, char *code, param *p) {

  int	evvar, i, is_var, c = 0;
  char	*varname, *varval;

  while (ocml__strpos (code, VAR_PREFIX, 0) != -1) { /* solange Variablen im Code sind */

    varname = (char *)ap_pcalloc(r->pool, sizeof(char *));

    evvar = ocml__strpos (code, VAR_PREFIX, 0); /* Position der ersten Variablen finden */

    /* **** Variablennamen feststellen **** */

    for (i = evvar + 1; i < strlen (code); i++) {

      is_var = 0;
  
      switch (code[i]) {
        case 'A' ... 'Z': is_var = 1;	/* literals and numbers are allowed */
        case 'a' ... 'z': is_var = 1;	/* as variable name		    */
        case '1' ... '9': is_var = 1;
      } // end switch  

      if (is_var == 1)

        varname[strlen(varname)] = code[i];

      else
      
        i = strlen (code) - 1; // for-Schleife abbrechen

    } // end for

    /* **** Code entsprechend modifizieren **** */

    if (strlen (varname) > 0) {

      /* get our var from array */

      varval = ocml__get_val_for_key (r, p, varname);

      /* default value */
      
      if (varval == NULL) {
	c++;	/* output warning only once */
      	if (c == 1)
          ap_rprintf (r, "<b>mod_ocaml Parser Warning:</b> Undefined variable(s) within code. OCAML could have bailed out after the first one.<br>\n");
        varval = VAR_UNDEF_VALUE;
      }
      
      /* if var-content-length > var-name-length => shift string right to fit */

      if (strlen(varval) > strlen(varname) + 1)

        code = ocml__strshr (r, code, evvar + strlen(varname), strlen(varval) - strlen (varname) - 1);

      /* if var-content-length < var-name-length => remove some chars to fit */

      else if (strlen(varval) < strlen(varname) + 1)
	code = ocml__strremove (r, code, evvar, strlen(varname) + 1 - strlen(varval));

      /* Variable evaluieren und einsetzen */

      for (i = evvar; i < evvar + strlen(varval); i++)
        code[i] = varval[i-evvar];

    } // end if

  } // end while

  return code;
  
} // ocml__evaluate_vars ()

/* *************************************************************************
   ocml__parse_ocml () - Code Parser - parse the incoming *code
   ************************************************************************* */

char *ocml__parse_ocml (request_rec *r, param *p) {

  int		type = UNDEF, return_parsed = 0, parse_until;
  char		*final_tag, *current_part;
  extern char	*glob_orig_code;

  /* determine type of code in current part */

  if (ocml__strpos (glob_orig_code, OCMLBLOCK_END, 0) == 0) { /* next to parse is of type HTML */
    type      = IS_HTML_BLOCK;
    final_tag = OCMLBLOCK_START;
  } // end if
  else if (ocml__strpos (glob_orig_code, OCMLBLOCK_START, 0) == 0) { /* ok, we parse OCAML now */
    type      = IS_OCAML_BLOCK;
    final_tag = OCMLBLOCK_END;
  } // end else if

  /* take action */

  if (type != UNDEF) {

    parse_until = ocml__strpos (glob_orig_code, final_tag, 0);

    /* now parse as far as necessary */
    current_part = ap_pstrndup (r->pool, glob_orig_code, parse_until);

    /* **** HTML part treatment **** */

    if ((type == IS_HTML_BLOCK) && (parse_until > strlen(OCMLBLOCK_END))) {
								    /* HTML  */
      return_parsed = 1;

      /* remove init tag */
      current_part = ocml__strremove (r, current_part, 0, strlen (OCMLBLOCK_END));
      /* escape chars from HTML_ESCAPE_CHARS */
      current_part = ocml__escape_chars (r, current_part, HTML_ESCAPE_CHARS);
      /* insert printf()-Statements for OCAML interpretability */
      current_part = ocml__sputonbegin (r, current_part, HTMLBLOCK_START);
      current_part = ocml__sputonend (r, current_part, HTMLBLOCK_END);

    } // end if
    
    else if (type == IS_OCAML_BLOCK) {				    /* OCAML */
    
      return_parsed = 1;
    
      /* remove init tag */
      current_part = ocml__strremove (r, current_part, 0, strlen (OCMLBLOCK_START));
      /* evaluate vars */
      current_part = ocml__evaluate_vars (r, current_part, p);

    } // end else if

    /* remove already-parsed part from original code */
    glob_orig_code = ocml__strremove (r, glob_orig_code, 0, parse_until);

    if (return_parsed == 1)
      return current_part;		/* return parsed code */

    else
      return "";			/* there was nothing to parse */

  } // end if (type != UNDEF)
  
  return "";				/* you should never get here */

} // ocml__parse_ocml ()

/* *************************************************************************
   ocml__do_ocamlize () - Fully OCAMLize a given file and pass to interpreter
   ************************************************************************* */

char *ocml__do_ocamlize (request_rec *r, param *p) {

  extern char	*glob_orig_code;
  char		*input_file, *code_part, *parsed_code, *cc;
  int		in_ret;
  FILE		*output_stream;
  
  char		*parsed_file = ocml__generate_random_filename (r, TMP_FILE_DIR, PARSED_FILE_EXT);
  char		*output_file = ocml__generate_random_filename (r, TMP_FILE_DIR, OUTPUT_FILE_EXT);

  input_file = ap_pstrcat (r->pool, r->filename, r->path_info, NULL);

  /* read input file */

  glob_orig_code = ocml__read_ocml (r, input_file);

  if (glob_orig_code == NULL) return "";		/* empty file	*/

  /* check nesting */

  if (ocml__strcnt (r, glob_orig_code, OCMLBLOCK_START) != ocml__strcnt (r, glob_orig_code, OCMLBLOCK_END)) {
    ap_rprintf (r, "<b>mod_ocaml Parser Error:</b> Incorrect nesting of <em>&lt;?ocml</em> and <em>?&gt;</em> tags.<br>\n");
    return "";
  } // end if

  /* "Schiffchen" am Code-Anfang und -Ende */

  glob_orig_code = ocml__sputonbegin (r, glob_orig_code, OCMLBLOCK_END);
  glob_orig_code = ocml__sputonend   (r, glob_orig_code, OCMLBLOCK_START);

  parsed_code = (char *)ap_pcalloc(r->pool, sizeof (char *));

  /* parse file */

  while (strcmp (glob_orig_code, OCMLBLOCK_START) != 0) { /* this will be left at end */
 
    code_part = (char *)ap_pcalloc(r->pool, sizeof (char *));
    code_part = ocml__parse_ocml (r, p);

    parsed_code = ap_pstrcat (r->pool, parsed_code, code_part, NULL);

  } // end while (parsing code)

  /* write parsed source to file */

  ocml__write_ocml (r, parsed_file, parsed_code);

  /* call interpreter and redirect stderr */

  in_ret = system (ap_psprintf (r->pool, "%s %s >%s 2>&1", ap_escape_shell_cmd (r->pool, glob_interpreter_path), ap_escape_shell_cmd (r->pool, parsed_file), ap_escape_shell_cmd (r->pool, output_file)));

  if (in_ret != 0) {
    ap_rprintf (r, "<b>mod_ocaml Parser Warning:</b> OCAML interpreter bailed out with system error code %d.\n<br>", in_ret);
    ap_rprintf (r, "<pre>%s</pre>\n\n", ap_escape_html (r->pool, parsed_code));
  }

  /* send interpreter output to client */

  output_stream = ap_pfopen (r->pool, output_file, "r");
  ap_send_fd (output_stream, r);
  ap_pfclose (r->pool, output_stream);

  /* delete temporary files */

  unlink (output_file);
  unlink (parsed_file);

  return "";

} // ocml__do_ocamlize ()

/* *************************************************************************
   ocml__module_init ()
   ************************************************************************* */

static void ocml__module_init (server_rec *server, pool *p) {

  ap_add_version_component (MOD_OCAML_VERSION_INFO);

} // ocml__module_init ()

/* *************************************************************************
   ocml__module_parser ()
   ************************************************************************* */

static int ocml__module_parser (request_rec *r) {

	param	*p;
	char	*buf = (char *)ap_pcalloc(r->pool, sizeof (char *) * 255);

	/* return HTTP Error 404 if source file cannot be found */

	if (r->finfo.st_mode == 0) return NOT_FOUND;

	ap_setup_client_block (r, REQUEST_CHUNKED_DECHUNK);	

	/* read GET data ... */
	if (r->method_number == M_GET)
	
		p = ocml__parse_cgi_args (r, r->args);
	
	/* ... or POST data */
	else if ((r->method_number == M_POST) && (ap_should_client_block (r))) {
	
		ap_get_client_block (r, buf, 255);

		p = ocml__parse_cgi_args (r, buf);

	} // end else if

	else
	
		return DECLINED;

	/* SEND HTML - HEADER */
	
	r->content_type = "text/html";

	ap_send_http_header( r );

	/* SEND OCAMLIZED CODE */

	ap_rprintf (r, ocml__do_ocamlize (r, p));
	
	/* TELL APACHE EVERYTHING IS OK */
	
	return OK;

} // ocml__module_parser ()

/* *************************************************************************
   ocml__set_interpreter_path ()
   ************************************************************************* */

static const char *ocml__set_interpreter_path (cmd_parms *cmd, void *dummy, char *arg) {

  extern char *glob_interpreter_path;
  glob_interpreter_path = arg;
  return NULL;

} // ocml__set_interpreter_path ()

/* *************************************************************************
   ocml__module_handlers () - Define apache handlers
   ************************************************************************* */

static const handler_rec ocml__module_handlers[] =
{
	{"ocaml-parsed", ocml__module_parser },
	{ NULL }

};

/* *************************************************************************
   ocml__module_commands () - Define own httpd.conf directives
   ************************************************************************* */

static const command_rec ocml__module_commands[] = {
  { "OcamlInterpreterPath" , ocml__set_interpreter_path, NULL, RSRC_CONF, TAKE1, "OcamlInterpreterPath path" },
  { NULL }
};


/* *************************************************************************
   Callback Definitions
   ************************************************************************* */

module AP_MODULE_DECLARE_DATA mod_ocaml = {
STANDARD20_MODULE_STUFF,
ocml__module_init, /* Init */
NULL,
NULL,
NULL, /* Server Config */
NULL,
ocml__module_commands, /* CMD Table */
ocml__module_handlers, /* mod_ocaml default handler */
NULL,
NULL,
NULL, 
NULL, /* check access */
NULL,
NULL,
NULL, /* Logger */
NULL, /* header parser */
NULL,
NULL,
NULL
};

/* EOF (END-OF-FILE) */
