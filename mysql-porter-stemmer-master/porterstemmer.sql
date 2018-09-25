

DROP TABLE IF EXISTS `tblporterstemming`;
CREATE TABLE `tblporterstemming` (
  `Step` int(11) NOT NULL,
  `Ordering` int(11) NOT NULL,
  `phrase1` varchar(15) NOT NULL,
  `phrase2` varchar(15) NOT NULL DEFAULT '',
  PRIMARY KEY (`Step`,`Ordering`,`phrase1`,`phrase2`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


LOCK TABLES `tblporterstemming` WRITE;
/*!40000 ALTER TABLE `tblporterstemming` DISABLE KEYS */;
INSERT INTO `tblporterstemming` VALUES (1,0,'sses','ss'),(1,1,'ies','i'),(1,2,'ss','ss'),(1,3,'s',''),(2,0,'ational','ate'),(2,1,'tional','tion'),(2,2,'enci','ence'),(2,3,'anci','ance'),(2,4,'izer','ize'),(2,5,'bli','ble'),(2,6,'alli','al'),(2,7,'entli','ent'),(2,8,'eli','e'),(2,9,'ousli','ous'),(2,10,'ization','ize'),(2,11,'ation','ate'),(2,12,'ator','ate'),(2,13,'alism','al'),(2,14,'iveness','ive'),(2,15,'fulness','ful'),(2,16,'ousness','ous'),(2,17,'aliti','al'),(2,18,'iviti','ive'),(2,19,'biliti','ble'),(2,20,'logi','log'),(3,0,'icate','ic'),(3,1,'ative',''),(3,2,'alize','al'),(3,3,'iciti','ic'),(3,4,'ical','ic'),(3,5,'ful',''),(3,6,'ness',''),(4,0,'al',''),(4,1,'ance',''),(4,2,'ence',''),(4,3,'er',''),(4,4,'ic',''),(4,5,'able',''),(4,6,'ible',''),(4,7,'ant',''),(4,8,'ement',''),(4,9,'ment',''),(4,10,'ent',''),(4,11,'ion',''),(4,12,'ou',''),(4,13,'ism',''),(4,14,'ate',''),(4,15,'iti',''),(4,16,'ous',''),(4,17,'ive',''),(4,18,'ize','');
/*!40000 ALTER TABLE `tblporterstemming` ENABLE KEYS */;
UNLOCK TABLES;

DELIMITER ;;
CREATE FUNCTION `fnPorterAlgorithm`(variable_InWord varchar(4000)) RETURNS varchar(4000) CHARSET utf8
BEGIN

    DECLARE variable_Ret varchar(4000);
	DECLARE variable_Temp varchar(4000);

    # DO some initial cleanup
    SET variable_Ret = TRIM(lower(variable_InWord));

    # only strings greater than 2 are stemmed
    IF length(variable_Ret) > 2 Then
	    SET variable_Ret = fnPorterStep1(variable_Ret);
	    SET variable_Ret = fnPorterStep2(variable_Ret);
	    SET variable_Ret = fnPorterStep3(variable_Ret);
	    SET variable_Ret = fnPorterStep4(variable_Ret);
	    SET variable_Ret = fnPorterStep5(variable_Ret);
	END if;

	#End of Porter's algorithm.........returning the word
    RETURN variable_Ret;
 
END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterContainsVowel`(variable_Word varchar(4000)) RETURNS bit(1)
BEGIN

#checking word to see if vowels are present
DECLARE variable_pattern varchar(4000);
DECLARE variable_ret bit;

SET variable_ret = 0;

IF length(variable_Word) > 0 Then
	#find out the CVC pattern
	SET variable_pattern = fnPorterCVCpattern(variable_Word);

	#check to see if the return pattern contains a vowel
	IF instr(variable_pattern,'v') > 0 Then
		SET variable_ret = 1;
	End if;
END if;

RETURN variable_Ret;

END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterCountm`(variable_Word varchar(4000)) RETURNS tinyint(4)
BEGIN

#A \consonant\ in a word is a letter other than A, E, I, O or U, and other
#than Y preceded by a consonant. (The fact that the term `consonant' is
#defined to some extent in terms of itself does not make it ambiguous.) So in
#TOY the consonants are T and Y, and in SYZYGY they are S, Z and G. If a
#letter is not a consonant it is a \vowel\.

#declaring local variables
DECLARE variable_pattern varchar(4000);
DECLARE variable_ret tinyint;
DECLARE variable_i int;
DECLARE variable_flag bit;

#initializing
SET variable_ret = 0;
SET variable_flag = 0;
SET variable_i = 1;

If length(variable_Word) > 0 Then
	#find out the CVC pattern
	SET variable_pattern = fnPorterCVCpattern(variable_Word);

	#counting the number of m's...
	WHILE variable_i <= length(variable_pattern) Do
		IF SUBSTRING(variable_pattern,variable_i,1) = 'v' OR variable_flag = 1 Then
			SET variable_flag = 1;
			IF SUBSTRING(variable_pattern,variable_i,1) = 'c' Then
				SET variable_ret = variable_ret + 1;
				SET variable_flag = 0;
			End if;
		END if;

		SET variable_i = variable_i + 1;
	END While;
END If;


RETURN variable_Ret;

END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterCVCpattern`(variable_Word varchar(4000)) RETURNS varchar(4000) CHARSET utf8
BEGIN

#local variables
DECLARE variable_Ret varchar(4000);
DECLARE variable_i int;

#checking each character to see if it is a consonent or a vowel. also inputs the information in const_vowel
SET variable_i = 1;
SET variable_Ret = '';
WHILE variable_i <= length(variable_Word) DO

	IF instr('aeiou', SUBSTRING(variable_Word,variable_i,1)) > 0 Then
		SET variable_Ret = concat(variable_Ret,'v');
	# if y is not the first character, only then check the previous character
	ELSEIF SUBSTRING(variable_Word,variable_i,1) = 'y' AND variable_i > 1 Then
            	#check to see if previous character is a consonent
		IF instr('aeiou', SUBSTRING(variable_Word,variable_i-1,1)) = 0 Then
		     SET variable_Ret = concat(variable_Ret,'v');
		ELSE
		     SET variable_Ret = concat(variable_Ret,'c');
	    END If;
	Else
		SET variable_Ret = concat(variable_Ret, 'c');
	End if;

	# increment the counter
	SET variable_i = variable_i + 1;

END WHILE;

    RETURN variable_Ret;

END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterEndsCVC`(variable_Word varchar(4000)) RETURNS bit(1)
BEGIN

#*o  - the stem ends cvc, where the second c is not W, X or Y (e.g. -WIL, -HOP).

#declaring local variables
DECLARE variable_pattern varchar(3);
DECLARE variable_ret bit;


SET variable_ret = 0;

#'check to see if atleast 3 characters are present
If length(variable_Word) >= 3 Then

	# find out the CVC pattern
	# we need to check only the last three characters
	SET variable_pattern = RIGHT(fnPorterCVCpattern(variable_Word),3);

	# check to see if the letters in str match the sequence cvc
	IF variable_pattern = 'cvc' AND instr('wxy', RIGHT(variable_Word,1)) = 0 Then
		SET variable_ret = 1;
    END if;
End if;

RETURN variable_Ret;


END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterEndsDoubleConsonant`(variable_Word varchar(4000)) RETURNS bit(1)
BEGIN


#checking whether word ends with a double consonant (e.g. -TT, -SS).

#declaring local variables
DECLARE variable_holds_ends varchar(2);
DECLARE variable_ret bit;
DECLARE variable_hold_third_last CHAR(1);

SET variable_ret = 0;
#first check whether the size of the word is >= 2
If length(variable_Word) >= 2 Then

	# extract 2 characters from right of str
	SET variable_holds_ends = Right(variable_Word, 2);
	# checking if both the characters are same and not double vowel
	IF SUBSTRING(variable_holds_ends,1,1) = SUBSTRING(variable_holds_ends,2,1) AND
	   instr('aaeeiioouu', variable_holds_ends) = 0 Then

		#if the second last character is y, and there are atleast three letters in str
		If variable_holds_ends = 'yy' AND length(variable_Word) > 2 Then

			# extracting the third last character
			SET variable_hold_third_last = LEFT(Right(variable_Word, 3),1);
			IF instr('aaeeiioouu',variable_hold_third_last) > 0 Then
			    SET variable_ret = 1;
			End if;
		ELSE
		    SET variable_ret = 1;
	    END if;
    END if;    
End if;
            
RETURN variable_Ret;



END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterEndsDoubleCVC`(variable_Word varchar(4000)) RETURNS bit(1)
BEGIN

#*o  - the stem ends cvc, where the second c is not W, X or Y (e.g. -WIL, -HOP).

#declaring local variables
DECLARE variable_pattern varchar(3);
DECLARE variable_ret bit;

SET variable_ret = 0;

#check to see if atleast 3 characters are present
IF length(variable_Word) >= 3 Then

  	# find out the CVC pattern
	# we need to check only the last three characters
	SET variable_pattern = RIGHT(fnPorterCVCpattern(variable_Word),3);

	# check to see if the letters in str match the sequence cvc
	IF variable_pattern = 'cvc' AND instr('wxy', RIGHT(variable_Word,1)) = 0 Then
		SET variable_ret = 1;
	End if;
END if;


RETURN variable_Ret;

END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterStep1`(variable_InWord varchar(4000)) RETURNS varchar(4000) CHARSET utf8
BEGIN


DECLARE variable_Ret varchar(4000);
DECLARE variable_Phrase1 varchar(15); 
DECLARE variable_Phrase2 varchar(15);

#declaring local variables
DECLARE variable_m tinyint;
DECLARE variable_Temp varchar(4000);
DECLARE variable_second_third_success bit;

DECLARE done INT DEFAULT FALSE;
DECLARE variable_CursorName CURSOR FOR 
	SELECT phrase1, phrase2 
	FROM tblPorterStemming 
	WHERE Step = 1 AND RIGHT(variable_Ret ,length(Phrase1)) = Phrase1
	ORDER BY Ordering;

DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;



    # DO some initial cleanup
    SET variable_Ret = variable_InWord;

/*STEP 1A

    SSES -> SS                         caresses  ->  caress
    IES  -> I                          ponies    ->  poni
                                       ties      ->  ti
    SS   -> SS                         caress    ->  caress
    S    ->                            cats      ->  cat
*/

OPEN variable_CursorName;
read_loop: LOOP
FETCH variable_CursorName INTO variable_Phrase1, variable_Phrase2;
	IF done THEN
		LEAVE read_loop;
	END IF;


	# TODO - VERIFY THAT THIS IS WORKING

	IF RIGHT(variable_Ret ,length(variable_Phrase1)) = variable_Phrase1 Then
		SET variable_Ret = concat(LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1)), variable_Phrase2);
		Set done = TRUE;
		#BREAK
	End if;



END LOOP read_loop;
CLOSE variable_CursorName;



#STEP 1B
#
#   If
#       (m>0) EED -> EE                     feed      ->  feed
#                                           agreed    ->  agree
#   Else
#       (*v*) ED  ->                        plastered ->  plaster
#                                           bled      ->  bled
#       (*v*) ING ->                        motoring  ->  motor
#                                           sing      ->  sing
#
#If the second or third of the rules in Step 1b is successful, the following
#is done:
#
#    AT -> ATE                       conflat(ed)  ->  conflate
#    BL -> BLE                       troubl(ed)   ->  trouble
#    IZ -> IZE                       siz(ed)      ->  size
#    (*d and not (*L or *S or *Z))
#       -> single letter
#                                    hopp(ing)    ->  hop
#                                    tann(ed)     ->  tan
#                                    fall(ing)    ->  fall
#                                    hiss(ing)    ->  hiss
#                                    fizz(ed)     ->  fizz
#    (m=1 and *o) -> E               fail(ing)    ->  fail
#                                    fil(ing)     ->  file
#
#The rule to map to a single letter causes the removal of one of the double
#letter pair. The -E is put back on -AT, -BL and -IZ, so that the suffixes
#-ATE, -BLE and -IZE can be recognised later. This E may be removed in step
#4.


#initializing 
SET variable_second_third_success = 0;

#(m>0) EED -> EE..else..(*v*) ED  ->(*v*) ING  ->
IF RIGHT(variable_Ret ,length('eed')) = 'eed' Then

	#counting the number of m#s
	SET variable_temp = LEFT(variable_Ret, length(variable_Ret) - length('eed'));
	SET variable_m = fnPorterCountm(variable_temp);

	If variable_m > 0 Then
		SET variable_Ret = concat(LEFT(variable_Ret, length(variable_Ret) - length('eed')), 'ee');
	End if;
ELSEIF RIGHT(variable_Ret ,length('ed')) = 'ed' Then
	#trim and check for vowel
	SET variable_temp = LEFT(variable_Ret, length(variable_Ret) - length('ed'));
	If fnPorterContainsVowel(variable_temp) = 1 Then
		SET variable_ret = LEFT(variable_Ret, length(variable_Ret) - length('ed'));
		SET variable_second_third_success = 1;
	End if;
ELSEIF RIGHT(variable_Ret ,length('ing')) = 'ing' Then
	#trim and check for vowel
	SET variable_temp = LEFT(variable_Ret, length(variable_Ret) - length('ing'));
	If fnPorterContainsVowel(variable_temp) = 1 Then
		SET variable_ret = LEFT(variable_Ret, length(variable_Ret) - length('ing'));
		SET variable_second_third_success = 1;
	End if;
End if;



#If the second or third of the rules in Step 1b is SUCCESSFUL, the following
#is done:
#
#    AT -> ATE                       conflat(ed)  ->  conflate
#    BL -> BLE                       troubl(ed)   ->  trouble
#    IZ -> IZE                       siz(ed)      ->  size
#    (*d and not (*L or *S or *Z))
#       -> single letter
#                                    hopp(ing)    ->  hop
#                                    tann(ed)     ->  tan
#                                    fall(ing)    ->  fall
#                                    hiss(ing)    ->  hiss
#                                    fizz(ed)     ->  fizz
#    (m=1 and *o) -> E               fail(ing)    ->  fail
#                                    fil(ing)     ->  file


IF variable_second_third_success = 1 Then            #If the second or third of the rules in Step 1b is SUCCESSFUL
	IF RIGHT(variable_Ret ,length('at')) = 'at' Then	#AT -> ATE
		SET variable_ret = concat(LEFT(variable_Ret, length(variable_Ret) - length('at')), 'ate');
	ELSEIF RIGHT(variable_Ret ,length('bl')) = 'bl' Then	#BL -> BLE
	    SET variable_ret = concat(LEFT(variable_Ret, length(variable_Ret) - length('bl')), 'ble');
	ELSEIF RIGHT(variable_Ret ,length('iz')) = 'iz'	Then #IZ -> IZE
	    SET variable_ret = concat(LEFT(variable_Ret, length(variable_Ret) - length('iz')), 'ize');
	ELSEIF fnPorterEndsDoubleConsonant(variable_Ret) = 1 Then /*(*d and not (*L or *S or *Z))-> single letter*/
		IF instr('lsz',RIGHT(variable_Ret,1)) = 0 Then
		    SET variable_ret = LEFT(variable_Ret, length(variable_Ret) - 1);
		End if;
	ELSEIF fnPorterCountm(variable_Ret) = 1 then        /*(m=1 and *o) -> E */
		IF fnPorterEndsDoubleCVC(variable_Ret) = 1 Then
		   SET variable_ret = concat(variable_Ret, 'e');
		End if;
	END if;
END if;
    
#####################################################
#
#STEP 1C
#
#    (*v*) Y -> I                    happy        ->  happi
#                                    sky          ->  sky
IF RIGHT(variable_Ret ,length('y')) = 'y' Then
	#trim and check for vowel
	SET variable_temp = LEFT(variable_Ret, length(variable_Ret)-1);

	IF fnPorterContainsVowel(variable_temp) = 1 Then
		SET variable_ret = concat(LEFT(variable_Ret, length(variable_Ret) - 1), 'i');
	End if;
END if;

RETURN variable_Ret;
 


END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterStep2`(variable_InWord varchar(4000)) RETURNS varchar(4000) CHARSET utf8
BEGIN


/*STEP 2

    (m>0) ATIONAL ->  ATE           relational     ->  relate
    (m>0) TIONAL  ->  TION          conditional    ->  condition
                                    rational       ->  rational
    (m>0) ENCI    ->  ENCE          valenci        ->  valence
    (m>0) ANCI    ->  ANCE          hesitanci      ->  hesitance
    (m>0) IZER    ->  IZE           digitizer      ->  digitize
Also,
    (m>0) BLI    ->   BLE           conformabli    ->  conformable

    (m>0) ALLI    ->  AL            radicalli      ->  radical
    (m>0) ENTLI   ->  ENT           differentli    ->  different
    (m>0) ELI     ->  E             vileli        - >  vile
    (m>0) OUSLI   ->  OUS           analogousli    ->  analogous
    (m>0) IZATION ->  IZE           vietnamization ->  vietnamize
    (m>0) ATION   ->  ATE           predication    ->  predicate
    (m>0) ATOR    ->  ATE           operator       ->  operate
    (m>0) ALISM   ->  AL            feudalism      ->  feudal
    (m>0) IVENESS ->  IVE           decisiveness   ->  decisive
    (m>0) FULNESS ->  FUL           hopefulness    ->  hopeful
    (m>0) OUSNESS ->  OUS           callousness    ->  callous
    (m>0) ALITI   ->  AL            formaliti      ->  formal
    (m>0) IVITI   ->  IVE           sensitiviti    ->  sensitive
    (m>0) BILITI  ->  BLE           sensibiliti    ->  sensible
Also,
    (m>0) LOGI    ->  LOG           apologi        -> apolog

The test for the string S1 can be made fast by doing a program switch on
the penultimate letter of the word being tested. This gives a fairly even
breakdown of the possible values of the string S1. It will be seen in fact
that the S1-strings in step 2 are presented here in the alphabetical order
of their penultimate letter. Similar techniques may be applied in the other
steps.
*/

#declaring local variables
    DECLARE variable_Ret varchar(4000);
	DECLARE variable_Temp varchar(4000);
    DECLARE variable_Phrase1 varchar(15);
	DECLARE variable_Phrase2 varchar(15);


DECLARE done INT DEFAULT FALSE;
DECLARE variable_CursorName CURSOR FOR #, variable_i int
	SELECT phrase1, phrase2 FROM tblPorterStemming 
	WHERE Step = 2 AND RIGHT(variable_Ret ,length(Phrase1)) = Phrase1
	ORDER BY Ordering;

DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;


#checking word
SET variable_Ret = variable_InWord;


OPEN variable_CursorName;
read_loop: LOOP
FETCH variable_CursorName INTO variable_Phrase1, variable_Phrase2;
	IF done THEN
		LEAVE read_loop;
	END IF;

	IF RIGHT(variable_Ret ,length(variable_Phrase1)) = variable_Phrase1 Then
		SET variable_temp = LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1));
		IF fnPorterCountm(variable_temp) > 0 Then
			SET variable_Ret = concat(LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1)), variable_Phrase2);
		End if;
		Set done = TRUE;
	END if;


END LOOP read_loop;
CLOSE variable_CursorName;


#retuning the word
RETURN variable_Ret;
 


END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterStep3`(variable_InWord varchar(4000)) RETURNS varchar(4000) CHARSET utf8
BEGIN


/*STEP 3
    (m>0) ICATE ->  IC              triplicate     ->  triplic
    (m>0) ATIVE ->                  formative      ->  form
    (m>0) ALIZE ->  AL              formalize      ->  formal
    (m>0) ICITI ->  IC              electriciti    ->  electric
    (m>0) ICAL  ->  IC              electrical     ->  electric
    (m>0) FUL   ->                  hopeful        ->  hope
    (m>0) NESS  ->                  goodness       ->  good
*/

#declaring local variables
DECLARE variable_Ret varchar(4000);
DECLARE variable_Temp varchar(4000);
DECLARE variable_Phrase1 varchar(15);
DECLARE variable_Phrase2 varchar(15);
DECLARE variable_i int;
DECLARE done INT DEFAULT FALSE;
DECLARE variable_CursorName CURSOR FOR #, variable_i int
	SELECT phrase1, phrase2 FROM tblPorterStemming 
	WHERE Step = 3 AND RIGHT(variable_Ret ,length(Phrase1)) = Phrase1
	ORDER BY Ordering;

DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

#checking word
SET variable_Ret = variable_InWord;

OPEN variable_CursorName;
read_loop: LOOP
FETCH variable_CursorName INTO variable_Phrase1, variable_Phrase2;
	IF done THEN
		LEAVE read_loop;
	END IF;

    # Do Step 2

	IF RIGHT(variable_Ret ,length(variable_Phrase1)) = variable_Phrase1 Then
	
		SET variable_temp = LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1));
		IF fnPorterCountm(variable_temp) > 0 Then
			SET variable_Ret = concat(LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1)), variable_Phrase2);
		End if;
		Set done = TRUE;
	END if;

END LOOP read_loop;
CLOSE variable_CursorName;


#retuning the word
RETURN variable_Ret;
 

END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterStep4`(variable_InWord varchar(4000)) RETURNS varchar(4000) CHARSET utf8
BEGIN
#STEP 4
#
#    (m>1) AL    ->                  revival        ->  reviv
#    (m>1) ANCE  ->                  allowance      ->  allow
#    (m>1) ENCE  ->                  inference      ->  infer
#    (m>1) ER    ->                  airliner       ->  airlin
#    (m>1) IC    ->                  gyroscopic     ->  gyroscop
#    (m>1) ABLE  ->                  adjustable     ->  adjust
#    (m>1) IBLE  ->                  defensible     ->  defens
#    (m>1) ANT   ->                  irritant       ->  irrit
#    (m>1) EMENT ->                  replacement    ->  replac
#    (m>1) MENT  ->                  adjustment     ->  adjust
#    (m>1) ENT   ->                  dependent      ->  depend
#    (m>1 and (*S or *T)) ION ->     adoption       ->  adopt
#    (m>1) OU    ->                  homologou      ->  homolog
#    (m>1) ISM   ->                  communism      ->  commun
#    (m>1) ATE   ->                  activate       ->  activ
#    (m>1) ITI   ->                  angulariti     ->  angular
#    (m>1) OUS   ->                  homologous     ->  homolog
#    (m>1) IVE   ->                  effective      ->  effect
#    (m>1) IZE   ->                  bowdlerize     ->  bowdler
#
#The suffixes are now removed. All that remains is a little tidying up.

DECLARE variable_Ret varchar(4000);
DECLARE variable_Temp varchar(4000);
DECLARE variable_Phrase1 varchar(15);

DECLARE done INT DEFAULT FALSE;
DECLARE variable_CursorName CURSOR FOR #, variable_i int
	SELECT phrase1 FROM tblPorterStemming 
	WHERE Step = 4 AND RIGHT(variable_Ret ,length(Phrase1)) = Phrase1
	ORDER BY Ordering;

DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;



#checking word
    SET variable_Ret = variable_InWord;

OPEN variable_CursorName;
read_loop: LOOP
FETCH variable_CursorName INTO variable_Phrase1;
	IF done THEN
		LEAVE read_loop;
	END IF;

    # Do Step 4
	IF RIGHT(variable_Ret ,length(variable_Phrase1)) = variable_Phrase1 Then
		SET variable_temp = LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1));
		IF fnPorterCountm(variable_temp) > 1 Then
			IF RIGHT(variable_Ret ,length('ion')) = 'ion' Then
				IF RIGHT(variable_temp ,1) = 's' OR RIGHT(variable_temp ,1) = 't' Then
					SET variable_Ret = LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1));
				End if;
			ELSE
				SET variable_Ret = LEFT(variable_Ret, length(variable_Ret) - length(variable_Phrase1));
			End if;
		END if;
		Set done = TRUE;
	END if;

END LOOP read_loop;
CLOSE variable_CursorName;

#retuning the word
RETURN variable_Ret;
 

END ;;

DELIMITER ;;
CREATE FUNCTION `fnPorterStep5`(variable_InWord varchar(4000)) RETURNS varchar(4000) CHARSET utf8
BEGIN

#STEP 5a
#
#    (m>1) E     ->                  probate        ->  probat
#                                    rate           ->  rate
#    (m=1 and not *o) E ->           cease          ->  ceas
#
#STEP 5b
#
#    (m>1 and *d and *L) -> single letter
#                                    controll       ->  control
#                                    roll           ->  roll

#declaring local variables
DECLARE variable_Ret varchar(4000); 
DECLARE variable_Temp varchar(4000);
DECLARE variable_m tinyint;

SET variable_Ret = variable_InWord;

#Step5a
IF RIGHT(variable_Ret , 1) = 'e' then	            #word ends with e
	SET variable_temp = LEFT(variable_Ret, length(variable_Ret) - 1);
	SET variable_m = fnPorterCountm(variable_temp);
	IF variable_m > 1 Then						#m>1
		SET variable_Ret = LEFT(variable_Ret, length(variable_Ret) - 1);
	ELSEIF variable_m = 1 Then				#m=1
		IF fnPorterEndsCVC(variable_temp) = 0 Then		#not *o
			SET variable_Ret = LEFT(variable_Ret, length(variable_Ret) - 1);
		End if;
	END if;
END if;
#####################################################
#
#Step5b
IF fnPorterCountm(variable_Ret) > 1 Then
	IF fnPorterEndsDoubleConsonant(variable_Ret) = 1 AND RIGHT(variable_Ret, 1) = 'l' Then
	    SET variable_Ret = LEFT(variable_Ret, length(variable_Ret) - 1);
	End if;
END if;

#retuning the word
RETURN variable_Ret;


END ;;
DELIMITER ;
