//! # Implements Zobrist hashing for board states
//! 
//! Zobrist keys are initialized randomly
//! ZHash -- u64 hash type
//! 
//! ## Didactic note: 
//! Zobrist hashes for two identical positions are the same ONLY if obtained through any combination
//! of toggles from the SAME state. Given position A and B, if they both lead to C through m1..mn,
//! ZH(C(A)) == ZH(C(B))   <=>     ZH(B) is obtained from ZH(A) through some sequence of moves.
//! 
//! Building ZH(A) and ZH(B) independently by summing material score WILL NOT produce the same hash

use crate::board_repr::*;
use crate::square::*;
use crate::piece::*;
use crate::castling_rights::*;

const PIECE_KEYS: [[u64; SQUARE_COUNT]; PIECE_COUNT] = [
    [17614855446833723417, 12536883398518993758, 3567904417499597023, 8814239751734716218,17222953837466845608, 12107826359820080530, 5779123018311973663, 14573725864802551446, 14479427104004705113, 4930438808887663722, 9203285848630196120, 15406282894788155370, 17572102267415404739, 4239892608168728731, 7206793217487235258, 7112337777195211945, 10354084514233975555, 5470913266476837097, 12100985009019359943, 5413041211497173176, 4830885062093629545, 6401208894299417198, 12167012557501460631, 8159751879110710509, 11752202050915605563, 1461647713612842020, 17941859366295334006, 18440953717808805462, 8942616487673140191, 9507921269313831560, 16370806428938600872, 1469379062509720930, 6511805760250708974, 12317689640223157525, 7044844722715314499, 4666441656052472827, 8141323950155823839, 9769480406085353984, 2016703756506026912, 4860874949811885591, 15154410414696931363, 5251154836523175949, 18091009696174051319, 7168789197883390283, 10374814050796813043, 4386107567477337679, 7787145914728197192, 6406670088948264768, 13325218115487654920, 4719967218624429881, 16471720396314013890, 10795380702092656222, 6669141538709382070, 5566021369591711450, 8521258879387999856, 13402570296019662922, 15116107930220971836, 13225696702670053497, 11264105313097181874, 12186319514817154522, 17540564200633342038, 2110367726917935504, 12923292109263864625, 14255852347765031773],
    [2085460877719934850, 16439315114002565693, 7081440181282338322, 3258974710337419924, 2503547728919351902, 8359947022543167782, 4009170310814817048, 6653098414222023809, 6666112716753972159, 17629546359388712294, 9355641483340629652, 15128309667130786087, 9227231734561511360, 7575258293725016108, 1529306249040092182, 18380934983579144032, 17458841076225421230, 10623716112622018040, 8425134479146985966, 3934509131442971904, 2680675870692607092, 1942200011788052271, 12833144458647268328, 9502680533783061196, 11596462891714233488, 13618532615941201473, 15511511954663732416, 16983584075691287935, 6278919947043559901, 12234490963920477749, 17931763713012456410, 10892245215069265714, 15783787565388742855, 18109192954302723259, 13165978548658479523, 1210706499305511627, 13772424655102811346, 6471053502171757142, 15311520213922957633, 1734808824903933303, 10793656015982521187, 7394624996274184249, 2575466300818632852, 5044544504226951661, 3784924133723285023, 12118634178164560721, 6631939411195473248, 3251111533311539958, 2925127129963149240, 2235052235192320172, 2713269618319315972, 3452525964470120021, 11183991358551666899, 13471514409827725419, 1875099611169561815, 16067961842172764949, 17329979276710620105, 13088963105108692883, 4931441188576032419, 3442056825165121755, 3693429784509329538, 8994376429266808523, 15243788230871095426, 715771326038625696], 
    [3203230215086244972, 17650441414141227560, 10966288941640916246, 7912400302633311478, 419990720888925877, 3650906101984564582, 2042025550046817319, 2709892923745517442, 9485570588627195599, 13253850481953259172, 15842654159495433875, 9979882877338777509, 13711240105863426469, 1987087132940609530, 8270196452588460779, 16183775236482551308, 7721671675681012165, 9140956063739218626, 4690585815118689746, 5250937219507912899, 7592394362746243059, 17941058398180402988, 6314176918124508409, 8863604694183301131, 12512375773064430347, 359903304402846698, 10115302476276069199, 17185597004573073164, 6072355776898417118, 18431947696961833512, 12300737289911982064, 11878468715531891453, 17522035857045216516, 5675828491972105671, 8198729319536872341, 5407022472665131969, 12780158216427450866, 16287610863272798276, 7162063486518546424, 7312510601453838100, 7958819207286441547, 11812808454477047359, 1940013840606840451, 4877305507868293582, 3025689836973205758, 8976764997506132809, 16784742382479875215, 15475524128923233942, 1244608318418143230, 15327943640602243173, 11632354809769608077, 2044107186117186484, 698693376746210844, 10883680312547378168, 417919527935270465, 5453849251488814940, 16143712704980650773, 4479926598456181672, 15074992693959765406, 15196468157207031253, 1838253581091386249, 16664328424889956654, 9654355927519163285, 14820894972666497674], 
    [18065527452323324770, 15030213821015338660, 6155494998671350982, 9516315729640739076, 7879060452676456025, 2148761783885853843, 10716620465553101790, 6780864981244138853, 16101406556518918904, 506690029655502356, 12512535516253455385, 4437544798322667867, 11060130419982689591, 11060085379320336639, 7116330980463693286, 6158792552579048093, 1410838221624294052, 5510758445548341145, 9771326948126033406, 15804423505525655422, 16263005102232414634, 10817681336195892395, 13202329437880285507, 1011007834235289284, 16789315449284001281, 15380536205511698197, 9062604486876344572, 16260697813717714738, 3469647653829094439, 13604497256083306528, 16204331675660353601, 1595035460812824559, 4150999806047590859, 17988153090002923811, 18275233853419722656, 17858175978690727111, 1719481573919785479, 9301542631596968085, 9280312292329728363, 7218567011121938475, 10160588740576846812, 14084135356158863930, 14080667054894345292, 8888484128686529251, 11429024915740744906, 17292202022881961265, 15983110356098409199, 12260340353425691102, 16302162026293697418, 885444642787194651, 14631986559003114288, 14200961271288883905, 104879023797834274, 2205324522967548402, 10781341079089905531, 16308790132374887762, 2268564239478522701, 14085400860682035478, 9234678453692152895, 16968209508209943328, 9437966511652716332, 8972406373323194165, 12122177603831322408, 14492169126148234298],
    [1185746466667437201, 10561938585593621066, 5226026821810526518, 1883748626328700072, 9223849392584088331, 14413272067677631543, 18345626578250749881, 15511844825875317361, 9014082677947689489, 7090527261426177765, 5504652027113723767, 13409726633662297449, 13771410502762429419, 9057368126017554552, 15693616033458756345, 3404313389325306674, 6757743697417328157, 6008449820937761734, 14273857301759595482, 4776050022309893986, 5899462170350031293, 13635165323913548818, 12122347789647228874, 17160373047759321209, 6537707183833689231, 406882606118586126, 1965083795610046403, 13724735761617954742, 8208391397976542299, 11778411036980888940, 10958492086663288720, 242073306488021059, 4731945383842463447, 13531325704230538430, 7607694305805690603, 11702953167742744361, 10502106565448362140, 13405097740072349321, 10834246479286360386, 7635234960936708195, 14098991947949961937, 15017196406617631785, 18112332141821895899, 12554149080773205996, 11768278466102645888, 5828969092230363244, 9685638734281849314, 6051448357499923044, 3502154335770137187, 10584376001758467197, 12594071328425011863, 17574004962458514311, 8393809428794870656, 13815009262325887107, 16024547761570744427, 14219161330019796294, 17328081097813294450, 16540330429638587693, 7157603405198014531, 12217344571365615215, 14996894503061787386, 4828302469622603620, 4561180643905294057, 2833561971027290395],
    [15421546790249360980, 181578596842738297, 9613824767367546977, 14839436149321529910, 15445571680459987146, 8670756289473482161, 13207583392259248016, 7450185700201643533, 8096338710531323132, 9150238956689164725, 1106999558675661155, 10627660368168814919, 17827638105458086915, 5549093098134398123, 2239643767478623707, 3041321913494741160, 17833899116203113858, 10694105738721166987, 10403457688257221495, 5756064045426548306, 13433986673408958357, 16358400513442428750, 8268813249060285841, 18059807130196039481, 4983594104266481990, 7625230754223651825, 7651667849807549940, 8413941247024321619, 12264482918369770975, 11239125762994725515, 11725432782800096770, 8913335184100556959, 4040021456387831415, 4890307641823688786, 11218920532109902578, 15298306152428290377, 871532558313875318, 14922992393398283521, 18146066869700370377, 15341441958444407604, 3867662500797444102, 17771332695499709668, 520818735756508574, 7948473955293256162, 11801149072160134062, 10927419344555703470, 2420266353292560162, 10192480592614158529, 18033621021799503073, 3492088224974365867, 16301511775467218820, 10228361534379269200, 2844184194629758543, 445188690710552225, 2222716814406764812, 7223157498599860797, 2364087973655495795, 1448954836604018416, 12946255407445020457, 12130512593188529679, 7393757847630564695, 18230669673480738410, 7398648889272493599, 13093658180722574277],
    [16676114924282719528, 13219056002509764846, 15862664937094960339, 2889809802787281536, 13498026379223284353, 15551414205020813125, 13774515001889002960, 16739777509583903268, 14617378178913410133, 4314351834430943723, 11854036291571627844, 8150445193273774352, 2460834532581631451, 10766640521056839269, 16778649139359694909, 3407504304003038066, 888781243827035582, 10567815422079170837, 2975893954924021902, 7286008711715847599, 17030837886401014864, 6445407084339538878, 7899580877662329748, 1159160970092568027, 18408861088357450103, 15353575748880820800, 10095234932919123166, 5165296919908321152, 8427219440639668500, 5271836898500737691, 9158470551472451580, 17004108053657465687, 14976861237245884806, 5511040689413167641, 6096127694437237043, 13682811262305697789, 14431515146738145351, 2059425279005353521, 17773042417310466341, 15027884464346542248, 17177586537836525987, 6611741309138944406, 663694344255941830, 3460957283898260585, 14395201453266024474, 17101235765360756941, 12136988402888889407, 2156272384604520906, 5896933207248743008, 16251853915447874396, 6711284786818620896, 13872666637820939145, 5696513458021940050, 10294449832436531462, 8820229942031957907, 4000249928673323346, 11697557449023923441, 17246453561131410792, 16469993547099986907, 6892471658716030924, 17095736519717675709, 12488994907607675161, 9029746374859381083, 4523082125790225456],
    [7693025732001649569, 18441040588559597171, 8508413637482273497, 12910885340012216489, 4593411926108585083, 17222611140260180543, 3236674830373623421, 9494956532383642531, 18308136173592650458, 625349702406268663, 9891465939878675893, 18159301481286473658, 15340375084861569355, 5042156430130590662, 18312140176539469321, 10290327250638571430, 7862071908418037687, 6915035116201661453, 17961927849477560380, 14621928195208421814, 8458518995496278060, 17324759053476767493, 8746869135061209819, 7269402554667811009, 7148275511601715314, 13846905235805951091, 15435959717133029478, 10589125358750067219, 499173286280665479, 11615035705290482453, 16122151265756352920, 16153580380440449798, 15509659943943467050, 15671850408110324748, 1480525869919506566, 3633778171465868901, 10257197174851553948, 4181148575875552363, 8138183034515857676, 14060199434134552063, 7021199016238904912, 4923606981122994337, 9733873647692733866, 10374683428741142440, 713914438933549482, 5130715328749006081, 9292958437075750325, 14259244076695419397, 11531432752412931993, 4069796937402855070, 3683590244510794266, 12041287085351122076, 9821942456649585621, 1211253165562034143, 14207068289401173921, 8951621824048659392, 15311156459997769501, 5773643947201587008, 10553786387257220203, 12490912225753992458, 13462937886145795294, 10072341548473601747, 1565648976375386068, 17918093883566271965],
    [10552833423627577893, 3548443034672821480, 16498232933254902926, 9321161997075131548, 17868899247276375890, 13071453567516307816, 16371361090034158677, 17323850598193605371, 4538911533056767286, 100933319478892763, 8431774363184997372, 12963013783303226345, 15024563877326112537, 10963864196057907106, 8922911678258664623, 12554584656104282701, 14848776911326046129, 8713551772422165680, 4172765177724058312, 17806138721627888326, 7924788593156711252, 9749601405280800368, 14762440884744316827, 1884803688077584618, 7820155271479014138, 8610637048986070495, 10405599764930953871, 5521891754378407039, 15462396997692142444, 15005660486390541761, 10141722291903818628, 9598280447104692949, 17971129912335493587, 4386854134645516962, 1463065281802455491, 4719556838971461001, 11414796182496730406, 13463435719137578343, 12260638421901698330, 18188531950280087994, 5057133116131125210, 13170396912117774743, 1472796310168234541, 13232738125487420786, 12599249297970342576, 16255431022111788559, 2246140080408540242, 1706300806226508447, 3553445393937615373, 6148279712471618754, 17277358807326361172, 5370048265569102895, 7272495254089263066, 3067036150500446692, 1481911612503584459, 16025048982330011431, 15044554216929512715, 12679302591152645915, 2951006263985547160, 4181279907242453054, 6253502554245920410, 11626624042681814214, 8583837646841595276, 2511362247781331291],
    [6011609301952459540, 984174930262399785, 5749970467562348121, 13238200665479050977, 5878691288744733031, 11605482540698618838, 17186901667595307418, 3002667294279676347, 10103602635001779271, 15167462410754053894, 9856907343835725994, 17303219235295935350, 15214454611750724234, 10918371167307600124, 15443906006986651437, 14743788549609686744, 15762320061750994474, 3753511258451058081, 15838577168722323245, 5358020568363595273, 3266442143660610698, 10377538760778356762, 11462536283782103709, 10834678515652577454, 9468695600154260771, 10187634445750014818, 4335586342509763033, 13284849217474357365, 11746518809114821195, 17637854719286675218, 17879928353906066257, 2044365794371680062, 7893169418306493544, 666519684515272163, 2535885248729070153, 7947911269004560993, 5188678630300261501, 15785610846235398309, 5782878579593952795, 4823996272945088588, 4173030110695543663, 18122778691016637658, 2146630413446459275, 9663888954828854712, 7536192439451834087, 18045315279731788283, 1085069686559842509, 12885413705641511548, 18150308132507740841, 7032571223136167275, 7758798198972875550, 17760950208083009898, 11749765588200141614, 4262167234237862731, 12111782886550578938, 13072707023824780811, 10549373877620050995, 14931035872246317268, 6106014568247640176, 14694750126158228926, 2105643230139760156, 7514503601084618330, 15372688770013294518, 12639001511714046810],
    [2301679502150308706, 12888245597485004314, 8472327426406460066, 16167135070201459342, 5294547189173395236, 1434310508320711666, 7297222155509751239, 1114504672984465818, 8004812792106191716, 4217293277146639806, 9920905834421076411, 14966038627305591953, 1736238509268120754, 15013801000894136063, 18131367744806665120, 15498383873827770008, 11204292087575589515, 11155289995599589909, 5763179666138748398, 13583254291694692597, 2770182198476530781, 4230217815341008853, 6642191561890141440, 13827580827885929028, 3100291840383367963, 27512479579670293, 15419130866500922530, 18218301983889171835, 13849598080973039935, 6357233123662461304, 9134527427452487768, 3236833120864696139, 1857927980583893669, 14699272180777484731, 10883381104998410736, 14122555777447838638, 1292451164299129316, 11560279624557315971, 802091427172774902, 16191389603193406419, 6686367510139226910, 17972537137910652497, 12953576894978715108, 8465616015950625951, 11266181255413715493, 1945665689242952371, 5775876898144857077, 8608463579417437138, 13516789548879184399, 3145371927030202066, 15271809491089208700, 2809447542892782536, 1232448865034533168, 10899066238181888050, 9428587169852783348, 12257853539762349053, 14781853219952508257, 1599910138332437371, 9156550606000948424, 8645017029226978351, 11351071897249759358, 2115304803662276124, 1911946987129340738, 17240671339630813641],
    [10677247755637399105, 11953896986240535315, 12026960717310882159, 4405836156653308190, 2933458505077475110, 4475035328119828938, 2284576059335210499, 2001096926044223274, 10485023105279697614, 12136119730421394340, 17822276716581761791, 14401361697847737958, 673875051465342537, 4012737693197705576, 3365317102678170544, 12139074654882021850, 17452826746603324549, 1662116992049192689, 8582152302215348155, 3251309837712512954, 16940908493942397468, 2696458440143616987, 17588722552525430646, 215779776827189203, 15003303740588919813, 17656635462679320113, 10378031352800666064, 17261496473118083215, 7038013579006537012, 16552021601087497867, 19743766068764608, 1001832043132772476, 2760806244809920187, 8311256098350292265, 5143975795030379, 1492010220853881054, 4966307771255662074, 5953804395542108923, 2928000034128510191, 3757177173290674229, 5788550067544976379, 6781912408623989555, 7743173340135016312, 17381598475667388370, 11074664293743516115, 4047886423153023322, 1084868190934962075, 13660326444256782482, 10317796943965897187, 9516550608993903726, 278182285179841446, 10405361079934716125, 14583504093280659995, 16377271865509468890, 3331239675786714472, 13159681742465493062, 15735827818120964390, 12132112179197965136, 5665198906933116719, 8172562078374105716, 2939921012984571344, 811875132513170841, 17590421507200302306, 7694804305820019536]
];
const EP_KEYS: [u64; SQUARE_COUNT] = [15924569520556073402, 9944164381255554503, 10054503023195131389, 4541907978338931007, 10225916827320380705, 5954954184641236995, 16054548534789906948, 3116605095982147011, 17866292765857938274, 7273439237118115991, 14567418774511616447, 4045686235803177881, 9633659909070738338, 11273779579349587321, 15632936572263276128, 1475859159877916555, 8524756528884403278, 9158562926495758857, 16996991434087075667, 14722093411987392480, 1085817608099876958, 8679642608668718559, 17960320860913967834, 706599713208366099, 16053482088918750126, 12004196259308896705, 15204186012226611446, 8888238931137964228, 6505428059228897501, 8731169615275603558, 8211090632751635364, 12318213379660375426, 9626510564461125113, 11576527661595977110, 491157628323964905, 1435660295244371894, 919131700201635885, 3063300577740449300, 16261099487995110882, 14587706000437279089, 10180526165664808586, 13162661105575531231, 10035136453587499047, 4633693018932020637, 4136023768346057011, 8141057679336880634, 7475868593315242276, 9715208222308373541, 7003337631514780291, 6370705591665519891, 8445811262683308362, 1601016785200906403, 9997136151609960515, 17764994441029343685, 1537427992313415970, 17860620340178556070, 1185038637364280995, 12151721184926660614, 10820218173697171129, 2528808240965128396, 1573531178936565745, 6864838588846900039, 2474532308771600493, 14599039209436570616];
const CASTLE_KEYS: [u64; CASTLE_COUNT] = [8406779754442449593, 871698248595409707, 8792824521864740815, 4589510442007724706, 4083044191740519165, 6740404646480981973, 15570744254755882244, 2329145575591560654, 510348464190612988, 9157648730753369883, 14254612341539302221, 8855270439616209896, 13893838058836631177, 12622557577655849614, 12093051889040103024, 1772028295636336235];
const SIDE_KEY: u64 = 4747071328949516916;


#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct ZHash (pub u64);
pub const NULL_HASH: ZHash = ZHash(0);
pub const START_HASH: ZHash = ZHash(11231077536533049824);

impl ZHash {
    pub fn new(board: &Board) -> ZHash {
        let mut hash: ZHash = ZHash(0);

        for piece in ALL_PIECES {
            for square in board.pieces[piece as usize] {
                hash.toggle_piece(piece, square);
            }
        }

        if let Some(square) = board.en_passant {
            hash.toggle_ep(square);
        }

        hash.toggle_castle(board.castling_rights);
        if board.side == Color::White {
            hash.toggle_side();
        }

        hash
    }

    #[inline(always)]
    pub fn toggle_piece(&mut self, piece: Piece, square: Square) {
        self.0 ^= PIECE_KEYS[piece as usize][square as usize];
    }

    #[inline(always)]
    pub fn move_piece(&mut self, piece: Piece, from: Square, to: Square) {
        self.0 ^= PIECE_KEYS[piece as usize][from as usize];
        self.0 ^= PIECE_KEYS[piece as usize][to as usize];
    }

    #[inline(always)]
    pub fn toggle_ep(&mut self, square: Square) {
        self.0 ^= EP_KEYS[square as usize];
    }
    
    #[inline(always)]
    pub fn toggle_castle(&mut self, castle: CastlingRights) {
        self.0 ^= CASTLE_KEYS[castle.index()];
    }

    #[inline(always)]
    pub fn swap_castle(&mut self, old_castle: CastlingRights, new_castle: CastlingRights) {
        self.0 ^= CASTLE_KEYS[old_castle.index()];
        self.0 ^= CASTLE_KEYS[new_castle.index()];
    }

    #[inline(always)]
    pub fn toggle_side(&mut self) {
        self.0 ^= SIDE_KEY;
    }

}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        tables::*,
        moves::Move
    };

    #[test]
    pub fn test_hash() {
        let b1: Board = Board::default();
        let b2: Board = Board::try_from(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        ).unwrap();

        assert_eq!(ZHash::new(&b1), START_HASH);   // correct start hash
        assert_eq!(ZHash::new(&b2), b2.hash);      // try_from() builds hash correctly
    }

    #[test]
    pub fn test_hash_castle() {
        // testing white king castling kingside
        let b1: Board = Board::try_from("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
        let m = Move::encode(Square::E1, Square::G1, Piece::WK, Piece::WP, Piece::WP, 0, 0, 0, 1);
        let b2 = b1.make_move(m, &Tables::default()).unwrap();

        let mut z1 = b1.hash;
        z1.toggle_piece(Piece::WK, Square::E1);
        z1.toggle_piece(Piece::WK, Square::G1);
        z1.toggle_piece(Piece::WR, Square::H1);
        z1.toggle_piece(Piece::WR, Square::F1);

        let old_rights = CastlingRights::try_from("KQkq").unwrap();
        let new_rights = CastlingRights::try_from("kq").unwrap();
        z1.swap_castle(old_rights, new_rights);
        z1.toggle_side();

        // z1 is the same as we obtained through incremental hash updates in make move
        assert_eq!(z1, b2.hash);
    }

    #[test]
    pub fn test_hash_enpassant() {
        // testing white pawn capturing en passant
        let b1: Board = Board::try_from("rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1").unwrap();
        let m = Move::encode(Square::F5, Square::E6, Piece::WP, Piece::BP, Piece::WP, 1, 0, 1, 0);
        let b2 = b1.make_move(m, &Tables::default()).unwrap();

        let mut z1 = b1.hash;
        z1.toggle_piece(Piece::WP, Square::F5);
        z1.toggle_piece(Piece::WP, Square::E6);
        z1.toggle_piece(Piece::BP, Square::E5);

        z1.toggle_ep(Square::E6);
        z1.toggle_side();

        // z1 is the same as we obtained through incremental hash updates in make move
        assert_eq!(z1, b2.hash);
    }

    #[test]
    pub fn test_hash_null() {
        // testing null move
        let b1: Board = Board::try_from("rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1").unwrap();
        let b2 = b1.make_null_move();

        let mut z1 = b1.hash;

        z1.toggle_ep(Square::E6);
        z1.toggle_side();

        // z1 is the same as we obtained through incremental hash updates in make move
        assert_eq!(z1, b2.hash);
    }
}