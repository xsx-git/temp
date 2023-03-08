# 香色闺阁源自用笔记（纯小白）
- [一、请求头](#一请求头)
  - [1.常用请求头](#1常用请求头)
    - [苹果手机请求头](#苹果手机请求头)
    - [安卓手机请求头](#安卓手机请求头)
    - [PC请求头 - windows](#pc请求头---windows)
  - [2.其他请求头](#2其他请求头)
    - [PC完整请求头 - 图片内容用](#pc完整请求头---图片内容用)
    - [请求头常用参数](#请求头常用参数)
- [二、代码段](#二代码段)
  - [1.常用xpath](#1常用xpath)
    - [xpath包含](#xpath包含)
    - [xpath列表过滤](#xpath列表过滤)
    - [xpath章节列表之后的dd](#xpath章节列表之后的dd)
    - [xpath最新章节之前的dd](#xpath最新章节之前的dd)
  - [2.常用香色短句](#2常用香色短句)
    - [详情页地址](#详情页地址)
    - [正文地址](#正文地址)
    - [base64解码](#base64解码)
    - [MD5编码](#md5编码)
    - [全局缓存](#全局缓存)
    - [取全局缓存](#取全局缓存)
    - [下一页范例\[1\]](#下一页范例1)
    - [下一页范例\[2\]](#下一页范例2)
  - [3.常用JS代码/函数](#3常用js代码函数)
    - [净化](#净化)
    - [繁\>简](#繁简)
    - [图片文字替换](#图片文字替换)
    - [时间戳](#时间戳)
- [三、搜索相关](#三搜索相关)
  - [1.搜索请求](#1搜索请求)
    - [搜索请求模板](#搜索请求模板)
  - [2.标准解析](#2标准解析)
    - [正则标准解析](#正则标准解析)
    - [xpath标准解析](#xpath标准解析)
  - [3.全名跳转解析](#3全名跳转解析)
    - [正则解析全名跳转](#正则解析全名跳转)
    - [xpath解析全名跳转](#xpath解析全名跳转)
  - [4.ajax（请求\>解析\>请求...）](#4ajax请求解析请求)
    - [ajax解决搜索无作者](#ajax解决搜索无作者)
    - [ajax二次请求（先获取搜索参数，在进行搜索）](#ajax二次请求先获取搜索参数在进行搜索)
    - [ajax 跳转验证（链接验证）  -- 风华居/52格格](#ajax-跳转验证链接验证-----风华居52格格)
    - [ajax多种搜索结果合并 -- 铜钟](#ajax多种搜索结果合并----铜钟)
    - [ajax 三方搜索 --完本神站](#ajax-三方搜索---完本神站)
- [四、详情相关](#四详情相关)
  - [1.详情请求](#1详情请求)
    - [标准详情请求或空](#标准详情请求或空)
    - [ajax详情过验证 -- 风华居](#ajax详情过验证----风华居)
  - [2.详情解析](#2详情解析)
    - [正则详情解析](#正则详情解析)
    - [xpath详情解析](#xpath详情解析)
- [五、目录相关](#五目录相关)
  - [1.目录请求](#1目录请求)
    - [标准目录请求或空](#标准目录请求或空)
    - [ajax二次请求过目录验证码 - 版主123](#ajax二次请求过目录验证码---版主123)
  - [2.标准解析](#2标准解析-1)
    - [正则目录解析](#正则目录解析)
    - [xpath目录解析](#xpath目录解析)
  - [3.隐藏链接](#3隐藏链接)
    - [隐藏章节链接 -- 默默中文（正则+xpath处理）](#隐藏章节链接----默默中文正则xpath处理)
    - [隐藏章节链接 -- 海棠书屋（正则处理）](#隐藏章节链接----海棠书屋正则处理)
    - [没有目录情况（填个list传下去）](#没有目录情况填个list传下去)
- [六、正文相关](#六正文相关)
  - [1.正文请求](#1正文请求)
    - [标准正文请求或空](#标准正文请求或空)
  - [2.标准解析](#2标准解析-2)
    - [xpath正文解析](#xpath正文解析)
    - [正则正文解析方案1](#正则正文解析方案1)
    - [正则正文解析方案2](#正则正文解析方案2)
  - [3.todo 翻页解析](#3todo-翻页解析)
    - [todo 正则解析](#todo-正则解析)
    - [todo xpath解析](#todo-xpath解析)
  - [4.正文乱序范例](#4正文乱序范例)
    - [99藏书网](#99藏书网)
    - [版主123](#版主123)

### 一、请求头
#### 1.常用请求头
##### 苹果手机请求头
```javascript
{
    "Referer": "https://www.baidu.com",
    "user-agent": "Mozilla/5.0 (iPhone; CPU iPhone OS 15_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.6 Mobile/15E148 Safari/604.1",
}
```
##### 安卓手机请求头 
```javascript
{
    "Referer": "https://www.baidu.com",
    "user-agent": "Mozilla/5.0 (Linux; U; Android 12; zh-CN; V2183A Build/SP1A.210812.003) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/78.0.3904.108 Quark/5.9.3.228 Mobile Safari/537.36",
}
```
##### PC请求头 - windows
```javascript
{
    "Referer": "https://www.baidu.com",
    "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36 Edg/109.0.1518.78",
}
```
#### 2.其他请求头
##### PC完整请求头 - 图片内容用
```javascript
{
    "sec-ch-ua": "\"Chromium\";v=\"110\", \"Not A(Brand\";v=\"24\", \"Microsoft Edge\";v=\"110\"",
    "dnt": "1",
    "sec-ch-ua-mobile": "?0",
    "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36 Edg/110.0.1587.41",
    "sec-ch-ua-platform": "\"Windows\"",
    "accept": "image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8",
    "sec-fetch-site": "cross-site",
    "sec-fetch-mode": "no-cors",
    "sec-fetch-dest": "image",
    "accept-encoding": "gzip, deflate, br",
    "accept-language": "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"

}
```
##### 请求头常用参数
```javascript
{
    "Upgrade-Insecure-Requests": "1",
    "cookie":"",
}
```
### 二、代码段
#### 1.常用xpath
##### xpath包含
```
//a[contains(@id,"rb")]
```
##### xpath列表过滤
```
[position()>1][position()<last()]
```
##### xpath章节列表之后的dd
```
//dt[contains(., "章节列表")]/following-sibling::dd
```
##### xpath最新章节之前的dd
```
//dt[contains(., "最新章节")]/preceding-sibling::dd
```
#### 2.常用香色短句
##### 详情页地址
```
params.queryInfo.detailUrl
```
##### 正文地址
```
params.queryInfo.url
```
##### base64解码
```
params.nativeTool.base64Decode("234")
```
##### MD5编码
```
params.nativeTool.md5Encode("234")
```

##### 全局缓存
```
params.nativeTool.setCache("tag", "123")
```

##### 取全局缓存
```
params.nativeTool.getCache("tag")
```
##### 下一页范例[1]
><font face='KaiTi' >用于无法获取url情况</font>
```javascript
//a[contains(., "下一页")]
||@js:
if (result) {
    return `${params.queryInfo.url.replace(/.html/, "")}_${params.pageIndex}.html`
}
```
##### 下一页范例[2]
><font face='KaiTi' >拼接下一页链接</font>
```javascript
//a[contains(., "下一页")]/@href
||@js:
let pqu = params.queryInfo.url
return pqu.substring(0, pqu.lastIndexOf("/") + 1) + result
```

#### 3.常用JS代码/函数
##### 净化
```javascript
function ad(str) {
    let ad_reg = /(精校完本小说，尽在|请收藏).*?c.?o.?m|[\(|（]本章未完.*?[）|\)]|本章.{1,50}(?:加载完毕|继续阅读)|为您提供.{1,50}(?:最快更新|保存好书签.)|第.{1,10}章.{1,50}(请收藏.{0,3}|免费阅读.{0,3})|.{1,10}免?费?阅?读.*?请收藏?|正在手打中.{1,50}获取最新更新.|以下是为你提供的.{1,50}敬请欣赏！|看《.{1,50}浏览器输入.{1,50}查看?|看.*?章节.*?更新越快.|温馨提示.{1,50}阅读模式.|首发更新.*?本章未完.{1,2}|网页版章节.*?阅读最新内容|请退出转码页面.*?最新章节?|.{1,3}为你提供最快的.*?更新.{1,10}|.{1,10}提醒您.看?完?记?得?收?藏.*?(\S|\s).*?畅阅无阻.{0,4}|.{1,10}向你推荐他的其他作品.*?希望你也喜欢|.喜欢看.*?完整章节?|《收藏.*?完本小说》|.{0,2}(神|精).{0,2}(仙|华).{0,2}书.{0,2}阁.{1,50}(首发更新|无错首发|\s最新章节|最快更新).{0,2}|紧急通知.*?收藏书签.|@(神|精)?(仙|华)?书?阁|『章节有误.*?』|「.*?退?出?阅?读?模?式.*?」|百?度?搜?索.{1,50}(?:秒?更|首?发|打造你的书库.)|一?秒?记?住.{1,100}不?迷?路.|(。|！|，|!).{2,10}最新链接.{1,50}[“|\/]|【.*?(野*果*阅*读|换*源*app)(?:\S|\s)*?】|无错更新.|147小说|1秒记住.*?网.*?网址.*?。$|手机阅读.{1,50}m|(\(|（)http.*?html(\)|）)|a？h?z?w?x?\.c?o?m.{0,20}(秒更|首发)|a?h?1?2?3?z\.c?o?m|http:..m.ahzww.net|ahwzw.net.*?爱*好*中*文*网|.{0,7}s?h?e?n?x?i?a?n?f?a?k?a.t?o?p?|\(txt.*?www.*?com\)/gi
    return str.replace(ad_reg, "")
}
```
##### 繁>简
```javascrpit
function trs(str) {
    let zh_t = "錒皚藹礙愛噯嬡璦曖靄諳銨鵪骯襖奧媼驁鰲壩罷鈀擺敗唄頒辦絆鈑幫綁鎊謗剝飽寶報鮑鴇齙輩貝鋇狽備憊鵯賁錛繃筆畢斃幣閉蓽嗶潷鉍篳蹕邊編貶變辯辮芐緶籩標驃颮飆鏢鑣鰾鱉別癟瀕濱賓擯儐繽檳殯臏鑌髕鬢餅稟撥缽鉑駁餑鈸鵓補鈽財參蠶殘慚慘燦驂黲蒼艙倉滄廁側冊測惻層詫鍤儕釵攙摻蟬饞讒纏鏟產闡顫囅諂讖蕆懺嬋驏覘禪鐔場嘗長償腸廠暢倀萇悵閶鯧鈔車徹硨塵陳襯傖諶櫬磣齔撐稱懲誠騁棖檉鋮鐺癡遲馳恥齒熾飭鴟沖衝蟲寵銃疇躊籌綢儔幬讎櫥廚鋤雛礎儲觸處芻絀躕傳釧瘡闖創愴錘綞純鶉綽輟齪辭詞賜鶿聰蔥囪從叢蓯驄樅湊輳躥竄攛錯銼鹺達噠韃帶貸駘紿擔單鄲撣膽憚誕彈殫賧癉簞當擋黨蕩檔讜碭襠搗島禱導盜燾燈鄧鐙敵滌遞締糴詆諦綈覿鏑顛點墊電巔鈿癲釣調銚鯛諜疊鰈釘頂錠訂鋌丟銩東動棟凍崠鶇竇犢獨讀賭鍍瀆櫝牘篤黷鍛斷緞籪兌隊對懟鐓噸頓鈍燉躉奪墮鐸鵝額訛惡餓諤堊閼軛鋨鍔鶚顎顓鱷誒兒爾餌貳邇鉺鴯鮞發罰閥琺礬釩煩販飯訪紡鈁魴飛誹廢費緋鐨鯡紛墳奮憤糞僨豐楓鋒風瘋馮縫諷鳳灃膚輻撫輔賦復負訃婦縛鳧駙紱紼賻麩鮒鰒釓該鈣蓋賅桿趕稈贛尷搟紺岡剛鋼綱崗戇鎬睪誥縞鋯擱鴿閣鉻個紇鎘潁給亙賡綆鯁龔宮鞏貢鉤溝茍構購夠詬緱覯蠱顧詁轂鈷錮鴣鵠鶻剮掛鴰摑關觀館慣貫詿摜鸛鰥廣獷規歸龜閨軌詭貴劊匭劌媯檜鮭鱖輥滾袞緄鯀鍋國過堝咼幗槨蟈鉿駭韓漢闞絎頡號灝顥閡鶴賀訶闔蠣橫轟鴻紅黌訌葒閎鱟壺護滬戶滸鶘嘩華畫劃話驊樺鏵懷壞歡環還緩換喚瘓煥渙奐繯鍰鯇黃謊鰉揮輝毀賄穢會燴匯諱誨繪詼薈噦澮繢琿暉葷渾諢餛閽獲貨禍鈥鑊擊機積饑跡譏雞績緝極輯級擠幾薊劑濟計記際繼紀訐詰薺嘰嚌驥璣覬齏磯羈蠆躋霽鱭鯽夾莢頰賈鉀價駕郟浹鋏鎵蟯殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗諫縑戔戩瞼鶼筧鰹韉將漿蔣槳獎講醬絳韁膠澆驕嬌攪鉸矯僥腳餃繳絞轎較撟嶠鷦鮫階節潔結誡屆癤頜鮚緊錦僅謹進晉燼盡勁荊莖巹藎饉縉贐覲鯨驚經頸靜鏡徑痙競凈剄涇逕弳脛靚糾廄舊鬮鳩鷲駒舉據鋸懼劇詎屨櫸颶鉅鋦窶齟鵑絹錈鐫雋覺決絕譎玨鈞軍駿皸開凱剴塏愾愷鎧鍇龕閌鈧銬顆殼課騍緙軻鈳錁頷墾懇齦鏗摳庫褲嚳塊儈鄶噲膾寬獪髖礦曠況誆誑鄺壙纊貺虧巋窺饋潰匱蕢憒聵簣閫錕鯤擴闊蠐蠟臘萊來賴崍徠淶瀨賚睞錸癩籟藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫嵐欖斕鑭襤瑯閬鋃撈勞澇嘮嶗銠鐒癆樂鰳鐳壘類淚誄縲籬貍離鯉禮麗厲勵礫歷瀝隸儷酈壢藶蒞蘺嚦邐驪縭櫪櫟轢礪鋰鸝癘糲躒靂鱺鱧倆聯蓮連鐮憐漣簾斂臉鏈戀煉練蘞奩瀲璉殮褳襝鰱糧涼兩輛諒魎療遼鐐繚釕鷯獵臨鄰鱗凜賃藺廩檁轔躪齡鈴靈嶺領綾欞蟶鯪餾劉瀏騮綹鎦鷚龍聾嚨籠壟攏隴蘢瀧瓏櫳朧礱樓婁摟簍僂蔞嘍嶁鏤瘺耬螻髏蘆盧顱廬爐擄鹵虜魯賂祿錄陸壚擼嚕閭瀘淥櫨櫓轤輅轆氌臚鸕鷺艫鱸巒攣孿灤亂臠孌欒鸞鑾掄輪倫侖淪綸論圇蘿羅邏鑼籮騾駱絡犖玀濼欏腡鏍驢呂鋁侶屢縷慮濾綠櫚褸鋝嘸媽瑪碼螞馬罵嗎嘜嬤榪買麥賣邁脈勱瞞饅蠻滿謾縵鏝顙鰻貓錨鉚貿麼沒鎂門悶們捫燜懣鍆錳夢瞇謎彌覓冪羋謐獼禰綿緬澠靦黽廟緲繆滅憫閩閔緡鳴銘謬謨驀饃歿鏌謀畝鉬吶鈉納難撓腦惱鬧鐃訥餒內擬膩鈮鯢攆輦鯰釀鳥蔦裊聶嚙鑷鎳隉蘗囁顢躡檸獰寧擰濘苧嚀聹鈕紐膿濃農儂噥駑釹諾儺瘧歐鷗毆嘔漚謳慪甌盤蹣龐拋皰賠轡噴鵬紕羆鈹騙諞駢飄縹頻貧嬪蘋憑評潑頗釙撲鋪樸譜鏷鐠棲臍齊騎豈啟氣棄訖蘄騏綺榿磧頎頏鰭牽釬鉛遷簽謙錢鉗潛淺譴塹僉蕁慳騫繾槧鈐槍嗆墻薔強搶嬙檣戧熗錆鏘鏹羥蹌鍬橋喬僑翹竅誚譙蕎繰磽蹺竊愜鍥篋欽親寢鋟輕氫傾頃請慶撳鯖瓊窮煢蛺巰賕蟣鰍趨區軀驅齲詘嶇闃覷鴝顴權勸詮綣輇銓卻鵲確闋闕愨讓饒擾繞蕘嬈橈熱韌認紉飪軔榮絨嶸蠑縟銣顰軟銳蜆閏潤灑薩颯鰓賽傘毿糝喪騷掃繅澀嗇銫穡殺剎紗鎩鯊篩曬釃刪閃陜贍繕訕姍騸釤鱔墑傷賞坰殤觴燒紹賒攝懾設厙灄畬紳審嬸腎滲詵諗瀋聲繩勝師獅濕詩時蝕實識駛勢適釋飾視試謚塒蒔弒軾貰鈰鰣壽獸綬樞輸書贖屬術樹豎數攄紓帥閂雙誰稅順說碩爍鑠絲飼廝駟緦鍶鷥聳慫頌訟誦擻藪餿颼鎪蘇訴肅謖穌雖隨綏歲誶孫損筍蓀猻縮瑣鎖嗩脧獺撻闥鉈鰨臺態鈦鮐攤貪癱灘壇譚談嘆曇鉭錟頇湯燙儻餳鐋鏜濤絳討韜鋱騰謄銻題體屜緹鵜闐條糶齠鰷貼鐵廳聽烴銅統慟頭鈄禿圖釷團摶頹蛻飩脫鴕馱駝橢籜鼉襪媧膃彎灣頑萬紈綰網輞韋違圍為濰維葦偉偽緯謂衛諉幃闈溈潿瑋韙煒鮪溫聞紋穩問閿甕撾蝸渦窩臥萵齷嗚鎢烏誣無蕪吳塢霧務誤鄔廡憮嫵騖鵡鶩錫犧襲習銑戲細餼鬩璽覡蝦轄峽俠狹廈嚇硤鮮纖賢銜閑顯險現獻縣餡羨憲線莧薟蘚峴獫嫻鷴癇蠔秈躚廂鑲鄉詳響項薌餉驤緗饗蕭囂銷曉嘯嘵瀟驍綃梟簫協挾攜脅諧寫瀉謝褻擷紲纈鋅釁興陘滎兇洶銹繡饈鵂虛噓須許敘緒續詡頊軒懸選癬絢諼鉉鏇學謔澩鱈勛詢尋馴訓訊遜塤潯鱘壓鴉鴨啞亞訝埡婭椏氬閹煙鹽嚴巖顏閻艷厭硯彥諺驗厴贗儼兗讞懨閆釅魘饜鼴鴦楊揚瘍陽癢養樣煬瑤搖堯遙窯謠藥軺鷂鰩爺頁業葉靨謁鄴曄燁醫銥頤遺儀蟻藝億憶義詣議誼譯異繹詒囈嶧飴懌驛縊軼貽釔鎰鐿瘞艤蔭陰銀飲隱銦癮櫻嬰鷹應纓瑩螢營熒蠅贏穎塋鶯縈鎣攖嚶瀅瀠瓔鸚癭頦罌喲擁傭癰踴詠鏞優憂郵鈾猶誘蕕銪魷輿魚漁娛與嶼語獄譽預馭傴俁諛諭蕷崳飫閾嫗紆覦歟鈺鵒鷸齬鴛淵轅園員圓緣遠櫞鳶黿約躍鑰粵悅閱鉞鄖勻隕運蘊醞暈韻鄆蕓惲慍紜韞殞氳雜災載攢暫贊瓚趲鏨贓臟駔鑿棗責擇則澤賾嘖幘簀賊譖贈綜繒軋鍘閘柵詐齋債氈盞斬輾嶄棧戰綻譫張漲帳賬脹趙詔釗蟄轍鍺這謫輒鷓貞針偵診鎮陣湞縝楨軫賑禎鴆掙睜猙爭幀癥鄭證諍崢鉦錚箏織職執紙摯擲幟質滯騭櫛梔軹輊贄鷙螄縶躓躑觶鐘終種腫眾鍾謅軸皺晝驟紂縐豬諸誅燭矚囑貯鑄駐佇櫧銖專磚轉賺囀饌顳樁莊裝妝壯狀錐贅墜綴騅縋諄準著濁諑鐲茲資漬諮緇輜貲眥錙齜鯔蹤總縱傯鄒諏騶鯫詛組鏃鉆纘躦鱒翺並蔔沈醜澱叠鬥範幹臯矽櫃後夥稭傑訣誇裏淩麽黴撚淒扡聖屍擡塗窪餵汙鍁鹹蠍彜湧遊籲禦願嶽雲竈紮劄築於誌註雕訁譾郤猛氹阪壟堖垵墊檾蕒葤蓧蒓菇槁摣咤唚哢噝噅撅劈謔襆嶴脊仿僥獁麅餘餷饊饢楞怵懍爿漵灩混濫瀦淡寧糸絝緔瑉梘棬案橰櫫軲軤賫膁腖飈糊煆溜湣渺碸滾瞘鈈鉕鋣銱鋥鋶鐦鐧鍩鍀鍃錇鎄鎇鎿鐝鑥鑹鑔穭鶓鶥鸌癧屙瘂臒襇繈耮顬蟎麯鮁鮃鮎鯗鯝鯴鱝鯿鰠鰵鱅鞽韝齇"
    let zh_s = "锕皑蔼碍爱嗳嫒瑷暧霭谙铵鹌肮袄奥媪骜鳌坝罢钯摆败呗颁办绊钣帮绑镑谤剥饱宝报鲍鸨龅辈贝钡狈备惫鹎贲锛绷笔毕毙币闭荜哔滗铋筚跸边编贬变辩辫苄缏笾标骠飑飙镖镳鳔鳖别瘪濒滨宾摈傧缤槟殡膑镔髌鬓饼禀拨钵铂驳饽钹鹁补钸财参蚕残惭惨灿骖黪苍舱仓沧厕侧册测恻层诧锸侪钗搀掺蝉馋谗缠铲产阐颤冁谄谶蒇忏婵骣觇禅镡场尝长偿肠厂畅伥苌怅阊鲳钞车彻砗尘陈衬伧谌榇碜龀撑称惩诚骋枨柽铖铛痴迟驰耻齿炽饬鸱冲冲虫宠铳畴踌筹绸俦帱雠橱厨锄雏础储触处刍绌蹰传钏疮闯创怆锤缍纯鹑绰辍龊辞词赐鹚聪葱囱从丛苁骢枞凑辏蹿窜撺错锉鹾达哒鞑带贷骀绐担单郸掸胆惮诞弹殚赕瘅箪当挡党荡档谠砀裆捣岛祷导盗焘灯邓镫敌涤递缔籴诋谛绨觌镝颠点垫电巅钿癫钓调铫鲷谍叠鲽钉顶锭订铤丢铥东动栋冻岽鸫窦犊独读赌镀渎椟牍笃黩锻断缎簖兑队对怼镦吨顿钝炖趸夺堕铎鹅额讹恶饿谔垩阏轭锇锷鹗颚颛鳄诶儿尔饵贰迩铒鸸鲕发罚阀珐矾钒烦贩饭访纺钫鲂飞诽废费绯镄鲱纷坟奋愤粪偾丰枫锋风疯冯缝讽凤沣肤辐抚辅赋复负讣妇缚凫驸绂绋赙麸鲋鳆钆该钙盖赅杆赶秆赣尴擀绀冈刚钢纲岗戆镐睾诰缟锆搁鸽阁铬个纥镉颍给亘赓绠鲠龚宫巩贡钩沟苟构购够诟缑觏蛊顾诂毂钴锢鸪鹄鹘剐挂鸹掴关观馆惯贯诖掼鹳鳏广犷规归龟闺轨诡贵刽匦刿妫桧鲑鳜辊滚衮绲鲧锅国过埚呙帼椁蝈铪骇韩汉阚绗颉号灏颢阂鹤贺诃阖蛎横轰鸿红黉讧荭闳鲎壶护沪户浒鹕哗华画划话骅桦铧怀坏欢环还缓换唤痪焕涣奂缳锾鲩黄谎鳇挥辉毁贿秽会烩汇讳诲绘诙荟哕浍缋珲晖荤浑诨馄阍获货祸钬镬击机积饥迹讥鸡绩缉极辑级挤几蓟剂济计记际继纪讦诘荠叽哜骥玑觊齑矶羁虿跻霁鲚鲫夹荚颊贾钾价驾郏浃铗镓蛲歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧谏缣戋戬睑鹣笕鲣鞯将浆蒋桨奖讲酱绛缰胶浇骄娇搅铰矫侥脚饺缴绞轿较挢峤鹪鲛阶节洁结诫届疖颌鲒紧锦仅谨进晋烬尽劲荆茎卺荩馑缙赆觐鲸惊经颈静镜径痉竞净刭泾迳弪胫靓纠厩旧阄鸠鹫驹举据锯惧剧讵屦榉飓钜锔窭龃鹃绢锩镌隽觉决绝谲珏钧军骏皲开凯剀垲忾恺铠锴龛闶钪铐颗壳课骒缂轲钶锞颔垦恳龈铿抠库裤喾块侩郐哙脍宽狯髋矿旷况诓诳邝圹纩贶亏岿窥馈溃匮蒉愦聩篑阃锟鲲扩阔蛴蜡腊莱来赖崃徕涞濑赉睐铼癞籁蓝栏拦篮阑兰澜谰揽览懒缆烂滥岚榄斓镧褴琅阆锒捞劳涝唠崂铑铹痨乐鳓镭垒类泪诔缧篱狸离鲤礼丽厉励砾历沥隶俪郦坜苈莅蓠呖逦骊缡枥栎轹砺锂鹂疠粝跞雳鲡鳢俩联莲连镰怜涟帘敛脸链恋炼练蔹奁潋琏殓裢裣鲢粮凉两辆谅魉疗辽镣缭钌鹩猎临邻鳞凛赁蔺廪檩辚躏龄铃灵岭领绫棂蛏鲮馏刘浏骝绺镏鹨龙聋咙笼垄拢陇茏泷珑栊胧砻楼娄搂篓偻蒌喽嵝镂瘘耧蝼髅芦卢颅庐炉掳卤虏鲁赂禄录陆垆撸噜闾泸渌栌橹轳辂辘氇胪鸬鹭舻鲈峦挛孪滦乱脔娈栾鸾銮抡轮伦仑沦纶论囵萝罗逻锣箩骡骆络荦猡泺椤脶镙驴吕铝侣屡缕虑滤绿榈褛锊呒妈玛码蚂马骂吗唛嬷杩买麦卖迈脉劢瞒馒蛮满谩缦镘颡鳗猫锚铆贸麽没镁门闷们扪焖懑钔锰梦眯谜弥觅幂芈谧猕祢绵缅渑腼黾庙缈缪灭悯闽闵缗鸣铭谬谟蓦馍殁镆谋亩钼呐钠纳难挠脑恼闹铙讷馁内拟腻铌鲵撵辇鲶酿鸟茑袅聂啮镊镍陧蘖嗫颟蹑柠狞宁拧泞苎咛聍钮纽脓浓农侬哝驽钕诺傩疟欧鸥殴呕沤讴怄瓯盘蹒庞抛疱赔辔喷鹏纰罴铍骗谝骈飘缥频贫嫔苹凭评泼颇钋扑铺朴谱镤镨栖脐齐骑岂启气弃讫蕲骐绮桤碛颀颃鳍牵钎铅迁签谦钱钳潜浅谴堑佥荨悭骞缱椠钤枪呛墙蔷强抢嫱樯戗炝锖锵镪羟跄锹桥乔侨翘窍诮谯荞缲硗跷窃惬锲箧钦亲寝锓轻氢倾顷请庆揿鲭琼穷茕蛱巯赇虮鳅趋区躯驱龋诎岖阒觑鸲颧权劝诠绻辁铨却鹊确阕阙悫让饶扰绕荛娆桡热韧认纫饪轫荣绒嵘蝾缛铷颦软锐蚬闰润洒萨飒鳃赛伞毵糁丧骚扫缫涩啬铯穑杀刹纱铩鲨筛晒酾删闪陕赡缮讪姗骟钐鳝墒伤赏垧殇觞烧绍赊摄慑设厍滠畲绅审婶肾渗诜谂渖声绳胜师狮湿诗时蚀实识驶势适释饰视试谥埘莳弑轼贳铈鲥寿兽绶枢输书赎属术树竖数摅纾帅闩双谁税顺说硕烁铄丝饲厮驷缌锶鸶耸怂颂讼诵擞薮馊飕锼苏诉肃谡稣虽随绥岁谇孙损笋荪狲缩琐锁唢睃獭挞闼铊鳎台态钛鲐摊贪瘫滩坛谭谈叹昙钽锬顸汤烫傥饧铴镗涛绦讨韬铽腾誊锑题体屉缇鹈阗条粜龆鲦贴铁厅听烃铜统恸头钭秃图钍团抟颓蜕饨脱鸵驮驼椭箨鼍袜娲腽弯湾顽万纨绾网辋韦违围为潍维苇伟伪纬谓卫诿帏闱沩涠玮韪炜鲔温闻纹稳问阌瓮挝蜗涡窝卧莴龌呜钨乌诬无芜吴坞雾务误邬庑怃妩骛鹉鹜锡牺袭习铣戏细饩阋玺觋虾辖峡侠狭厦吓硖鲜纤贤衔闲显险现献县馅羡宪线苋莶藓岘猃娴鹇痫蚝籼跹厢镶乡详响项芗饷骧缃飨萧嚣销晓啸哓潇骁绡枭箫协挟携胁谐写泻谢亵撷绁缬锌衅兴陉荥凶汹锈绣馐鸺虚嘘须许叙绪续诩顼轩悬选癣绚谖铉镟学谑泶鳕勋询寻驯训讯逊埙浔鲟压鸦鸭哑亚讶垭娅桠氩阉烟盐严岩颜阎艳厌砚彦谚验厣赝俨兖谳恹闫酽魇餍鼹鸯杨扬疡阳痒养样炀瑶摇尧遥窑谣药轺鹞鳐爷页业叶靥谒邺晔烨医铱颐遗仪蚁艺亿忆义诣议谊译异绎诒呓峄饴怿驿缢轶贻钇镒镱瘗舣荫阴银饮隐铟瘾樱婴鹰应缨莹萤营荧蝇赢颖茔莺萦蓥撄嘤滢潆璎鹦瘿颏罂哟拥佣痈踊咏镛优忧邮铀犹诱莸铕鱿舆鱼渔娱与屿语狱誉预驭伛俣谀谕蓣嵛饫阈妪纡觎欤钰鹆鹬龉鸳渊辕园员圆缘远橼鸢鼋约跃钥粤悦阅钺郧匀陨运蕴酝晕韵郓芸恽愠纭韫殒氲杂灾载攒暂赞瓒趱錾赃脏驵凿枣责择则泽赜啧帻箦贼谮赠综缯轧铡闸栅诈斋债毡盏斩辗崭栈战绽谵张涨帐账胀赵诏钊蛰辙锗这谪辄鹧贞针侦诊镇阵浈缜桢轸赈祯鸩挣睁狰争帧症郑证诤峥钲铮筝织职执纸挚掷帜质滞骘栉栀轵轾贽鸷蛳絷踬踯觯钟终种肿众锺诌轴皱昼骤纣绉猪诸诛烛瞩嘱贮铸驻伫槠铢专砖转赚啭馔颞桩庄装妆壮状锥赘坠缀骓缒谆准着浊诼镯兹资渍谘缁辎赀眦锱龇鲻踪总纵偬邹诹驺鲰诅组镞钻缵躜鳟翱并卜沉丑淀迭斗范干皋硅柜后伙秸杰诀夸里凌么霉捻凄扦圣尸抬涂洼喂污锨咸蝎彝涌游吁御愿岳云灶扎札筑于志注凋讠谫郄勐凼坂垅垴埯埝苘荬荮莜莼菰藁揸吒吣咔咝咴噘噼嚯幞岙嵴彷徼犸狍馀馇馓馕愣憷懔丬溆滟溷漤潴澹甯纟绔绱珉枧桊桉槔橥轱轷赍肷胨飚煳煅熘愍淼砜磙眍钚钷铘铞锃锍锎锏锘锝锪锫锿镅镎镢镥镩镲稆鹋鹛鹱疬疴痖癯裥襁耢颥螨麴鲅鲆鲇鲞鲴鲺鲼鳊鳋鳘鳙鞒鞴齄"
    let darr = zh_s.split("")
    zh_t.split("").forEach((x, k) => {
        let reg = new RegExp(x, "g")
        str = str.replace(reg, darr[k])
    })
    return str
}
```
##### 图片文字替换
```javascript
function trs(str) {
    let data = {'LioClJ': '体', '2NZ49v': '流'}     
    str = str.replace(/<.?strong>/gi,"")
    for (let key in data) {
        let reg = new RegExp(`<img src="${config.host}/wzbodyimg/${key}.png">`, "g")
        str = str.replace(reg, data[key])
    }
    return str
}
```
##### 时间戳
```javascript
Math.round(new Date() / 1000)
```
### 三、搜索相关
#### 1.搜索请求
##### 搜索请求模板

```javascript
@js:

let hp = {
    "searchkey": params.keyWord,
    "1": "1",
}
return {
    "url": "/search/",
    //"POST": true,
    "httpParams": hp,
    "httpHeaders": config.httpHeaders,
    //"forbidCookie": true,
    //"responseFormatType": "html",
    "cacheTime": 600,
}
```
#### 2.标准解析
##### 正则标准解析
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let list = []
    let reg = /正则/g
    while (tem = reg.exec(result)) {
        list.push({
            "bookName": tem[1],
            "author": tem[2],
            "detailUrl": tem[3],
        })
    }
    return {
        "list": list,
    }
}
```

##### xpath标准解析
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let list = []
    let xml = params.nativeTool.XPathParserWithSource(result)
    let list_xpath = xml.queryWithXPath(`列表`)
    for (i in list_xpath) {
        list.push({
            "bookName": xpath(list_xpath[i], `书名`),
            "author": xpath(list_xpath[i], `作者`).replace("辰东", "辰東"),
            "detailUrl": xpath(list_xpath[i], `链接`),
            // "cover": xpath(list_xpath[i], ``),
            // "desc": xpath(list_xpath[i], ``),
            // "cat": xpath(list_xpath[i], ``),
            // "status": xpath(list_xpath[i], ``),
            // "WordCount": xpath(list_xpath[i], ``),
            // "lastChapterTitle": xpath(list_xpath[i], ``),
        })
    }
    return {
        "list": list,
    }
    function xpath(item, path) {
        if (path.length > 0 && item.queryWithXPath(path).length > 0) {
            return item.queryWithXPath(path)[0].content()
        }
        return undefined
    }
}
```
#### 3.全名跳转解析
##### 正则解析全名跳转
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let list = []
    if (/链接关键字/.test(params.responseUrl)) {
        let reg = /搜索页正则/gi
        while (tem = reg.exec(result)) {
            list.push({
                "bookName": tem[1],
                "author": tem[2],
                "detailUrl": tem[3],
            })
        }
    } else {
        let reg = /详情页正则/i
        let tem = reg.exec(result)
        list.push({
            "bookName": tem[1],
            "author": tem[2],
            "detailUrl": params.responseUrl,
        })
    }
    return {
        "list": list,
    }
}
```

##### xpath解析全名跳转
```javascript  
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let list = []
    let xml = params.nativeTool.XPathParserWithSource(result)
    if (/链接关键字/.test(params.responseUrl)) {
        let list_xpath = xml.queryWithXPath(`列表`)
        for (i in list_xpath) {
            list.push({
                "bookName": xpath(list_xpath[i], `书名`),
                "author": xpath(list_xpath[i], `作者`).replace("辰东", "辰東"),
                "detailUrl": xpath(list_xpath[i], `链接`),
            })
        }
    } else {
        list.push({
            "bookName": xpath(xml, `书名`),
            "author": xpath(xml, `作者`).replace("辰东", "辰東"),
            "detailUrl": params.responseUrl,
        })
    }
    return {
        "list": list,
    }
    function xpath(item, path) {
        if (path.length > 0 && item.queryWithXPath(path).length > 0) {
            return item.queryWithXPath(path)[0].content()
        }
        return undefined
    }
}
```
#### 4.ajax（请求>解析>请求...）
##### ajax解决搜索无作者
```javascript
@js:

let hp = {
    "word": params.keyWord,
    "m": "2",
    "ChannelID": "0",
    "page": params.pageIndex,
}
let js = `
    let res = document.body.innerHTML.toString()
    let reg = />].*?href="(.*?)".*?>(.*?)<.a/gi
    let result = ""
    while (tem = reg.exec(res)) {
        let xhr = new XMLHttpRequest()
        xhr.open("GET", tem[1], false)
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
        xhr.overrideMimeType("text/html;charset=" + document.charset)
        xhr.send()
        result += "<i><a>" + tem[2] + "</a>" + "<b>" + xhr.responseText.match(/li>小说作者.(.*?)</)[1] + "</b>" + "<c>" + tem[1] + "</c></i>"
    }
    result
`
return {
    "url": "/search.asp",
    "webView": 1,
    "webViewJs": js,
    "webViewSkipUrls": ["js"],
    "httpParams": hp,
    //"forbidCookie": true,
    "httpHeaders": config.httpHeaders,
    "cacheTime": 600,
}
```

##### ajax二次请求（先获取搜索参数，在进行搜索）
```javascript
@js:

let hp = { 
    "key": params.keyWord,
}
let js = `
    //获取搜索关键字
    let key = document.URL.split("key=").pop()
    //获取二次请求的参数（3钟解析方式）
    let url = document.body.innerHTML.toString().match(/action="(.*?)"/)[1]
    // let url = document.getElementsByTagName("form")[0].action + "?searchtype=all&searchkey=" + key
    // let act = document.getElementsByName("act")[0].value
    let xhr = new XMLHttpRequest()
    xhr.open("POST", url, false)
    xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
    //xhr.overrideMimeType("text/html;charset=" + document.charset)
    xhr.overrideMimeType("text/csv;charset=gb2312")
    xhr.send("searchkey=" + key + "&searchtype=all&t_btnsearch=")
    xhr.responseText
`
return {
    "url": `/skin/common/sedeso.js`,
    "webView": 1,
    "webViewJs": js,
    //"webViewSkipUrls": ["js"],
    "httpParams": hp,
    "httpHeaders": config.httpHeaders,
    "cacheTime": 600,
}
```

##### ajax 跳转验证（链接验证）  -- 风华居/52格格
```javascript
@js:

let hp = { 
    "key": params.keyWord,
}
let js = `
    let result = ""
    
    let key = document.URL.split("key=").pop() //截取搜索关键字
    result = ajax("/search/")
    if(/anticc_js_concat/.test(result)){    //验证页面
        eval(result.match(/script'>(.*?);;"/)[1])
        result = ajax(url)
    }
    result
    function ajax(link){
        let xhr = new XMLHttpRequest()
        xhr.open("POST", link, false)
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
        xhr.send("searchkey=" + key + "&Submit=")
        return xhr.responseText
    }
`
return {
    "url": "/static/biquge/nocover.jpg",  //第一次请求无意义（避免无限跳转）
    "webView": 1,
    "webViewJs": js,
    "webViewSkipUrls": ["js"],
    "httpParams": hp,
    "httpHeaders": config.httpHeaders,
    "cacheTime": 600,
}
```

##### ajax多种搜索结果合并 -- 铜钟
```javascript
@js:

let url = "/api/exact_search"
let hp = {
    "keyword": params.keyWord,
}
let js = `
    let key = document.URL.split("keyword=").pop()
    //精确搜索结果
    let result = JSON.parse(document.getElementsByTagName("pre")[0].innerHTML)
    //模糊搜索
    let res = ajax_get("/api/fuzzy_search?keyword=" + key)
    if (res) {
        result.songs = result.songs.concat(res.songs)
    }
    //qq搜索
    res = ajax_get("/secondhand_api/search?platform=qq&keyword=" + key)
    if (res) {
        result.songs = result.songs.concat(res.data.songs)
    }
    //网易搜索
    res = ajax_get("/secondhand_api/search?platform=netease&keyword=" + key)
    if (res) {
        result.songs = result.songs.concat(res.data.songs)
    }
    //酷我搜索
    res = ajax_get("/secondhand_api/search?platform=kuwo&keyword=" + key)
    if (res) {
        result.songs = result.songs.concat(res.data.songs)
    }
    result
    function ajax_get(url) {
        let xhr = new XMLHttpRequest()
        xhr.open("GET", url, false)
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
        xhr.send()
        try {
            return JSON.parse(xhr.responseText)
        } catch (e) {
            return false
        }
    }
`
return {
    "url": url,
    "webView": true,
    "webViewJs": js,
    //"webViewSkipUrls": ["js"],
    "httpParams": hp,
    "httpHeaders": config.httpHeaders,
    "cacheTime": 600,
}
```

##### ajax 三方搜索 --完本神站
```javascript
@js:

let hp = {
    "q": `intitle:${params.keyWord}+site:wanben.org`,
}
let js = `
    let result = ""
    let list = []
    //搜索结果正则
    let reg = /_ctf=.{1,20}="(.{1,20}wanben.org\\/\\d+\\/).*?" h/gi
    while (tem = reg.exec(document.body.innerHTML.toString())) {
        //截取url
        tem[1] = tem[1].substring(0, tem[1].lastIndexOf("/") + 1)
        //排重
        if (list.length > 0 && list.indexOf(tem[1]) > -1){
            continue
        }
        list.push(tem[1])
        let xhr = new XMLHttpRequest()
        xhr.open("GET", tem[1], false)
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
        xhr.send()
        let r = xhr.responseText
        //拼接结果为xml格式
        result += "<i><a>" + r.match(/<h1>(.*?)</)[1] + "</a>" + "<b>" + r.match(/writer".*?">(.*?)</)[1] + "</b>" + "<c>" + tem[1] + "</c></i>"
    }
    if (result == ""){
        result = document.body.innerHTML.toString()
    }
    result
`
return {
    "url": "https://www.bing.com/search",
    "webView": 1,
    "webViewJs": js,
    "webViewSkipUrls": ["js"],
    "httpParams": hp,
    //"forbidCookie": true,
    "httpHeaders": config.httpHeaders,
    "cacheTime": 600,
}
```
### 四、详情相关
#### 1.详情请求
##### 标准详情请求或空
```javascript
@js:

return {
    "url": result,
    "httpHeaders": config.httpHeaders,
    //"forbidCookie": true,
    "cacheTime": 600,
}
```
##### ajax详情过验证 -- 风华居
```javascript
@js:

let js = `
    let result = document.body.innerHTML.toString()
    if(/anticc_js_concat/.test(result)){
        eval(result.match(/script'>(.*?);;"/)[1])
        result = ajax(url)
    }
    result
    function ajax(link){
        let xhr = new XMLHttpRequest()
        xhr.open("GET", link, false)
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
        xhr.send()
        return xhr.responseText
    }
`
return {
    "url": result,
    "webView": 1,
    "webViewJs": js,
    "webViewSkipUrls": ["js"],
    "httpHeaders": config.httpHeaders,
    "cacheTime": 600,
}
```
#### 2.详情解析
##### 正则详情解析
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let reg = /详情页正则/gi
    let tem = reg.exec(result)
    let pq = params.queryInfo
    pq.status = tem[1]
    pq.lastChapterTitle = tem[2]
    pq.wordCount = tem[3]
    pq.cover = tem[4]
    pq.cat = tem[5]
    pq.lastChapterTitle = tem[6]
    pq.desc = tem[7].replace(/\r|\n|<br.{0,2}>/g, "").replace(/&nbsp;/g, "")
    return pq
}
```
##### xpath详情解析
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let xml = params.nativeTool.XPathParserWithSource(result)

    let pq = params.queryInfo
    pq.cover = xpath(xml, `//*[@*="og:image"]/@content`)
    pq.desc = xpath(xml, `//*[@*="description"]/@content`).replace(/[\r\n]/g, "").replace(/&nbsp;/g, "")
    pq.cat = xpath(xml, `//*[@*="og:novel:category"]/@content`)
    pq.status = xpath(xml, `//*[@*="og:novel:status"]/@content`)
    pq.wordCount = xpath(xml, ``)
    pq.lastChapterTitle = xpath(xml, `//*[@*="og:novel:latest_chapter_name"]/@content`)
    return pq

    function xpath(item, path) {
        if (path.length > 0 && item.queryWithXPath(path).length > 0) {
            return item.queryWithXPath(path)[0].content()
        }
        return undefined
    }
}
```

### 五、目录相关
#### 1.目录请求
##### 标准目录请求或空
```javascript
@js:

return {
    "url": result.replace(/替换关键字/, ""),
    "httpHeaders": config.httpHeaders,
    //"forbidCookie": true,
    "cacheTime": 600,
}
```
##### ajax二次请求过目录验证码 - 版主123
```javascript
@js:

let aid = params.queryInfo.detailUrl.match(/(\d+)\/$/)[1]
let js = `
    let nc = document.createElement("canvas")
    nc.setAttribute("id", "canvas")
    document.body.appendChild(nc)
    let canvas = document.getElementById("canvas")
    let ctx = canvas.getContext("2d")
    let image = document.getElementsByTagName("img")[0]
    canvas.width = image.width
    canvas.height = image.height
    ctx.drawImage(image, 0, 0)
    let b64 = canvas.toDataURL()
    b64 = b64.split(",")[1]
    let key = document.URL.split("ttt=").pop()
    let xhr = new XMLHttpRequest()
    xhr.open("post", "https://wryy-llos.koyeb.app/api/ocr", false);
    xhr.send("img=" + b64 + "&type=bz")
    let code = xhr.responseText.trim()
    if (/验证码不对/.test(code)) {
        throw Error()
    }
    xhr.open("post", "/mytool/getChapterList/", false)
    xhr.overrideMimeType("text/html;charset=" + document.charset)
    xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
    xhr.send("verify=" + code + "&aid=${aid}&sbt=%E6%8F%90%E4%BA%A4")
    xhr.responseText
`
return {
    "url": "/mytool/getVerify/?d=" + Math.random(),
    "webView": "",
    "webViewJs": js,
    "webViewSkipUrls": ["js"],
    "tryCount": 3,
    "httpHeaders": config.httpHeaders,
    "cacheTime": 0,
}
```

#### 2.标准解析
##### 正则目录解析
```javascript
function functionName(config, params, result) {
    let list = []
    let reg = /页面解析正则/gi
    while (tem = reg.exec(result)) {
        list.push({
            "title": tem[2],
            "url": tem[1]
        })
    }
    let np = result.match(/下一页正则/)
    if (np.length > 0){
        return {
            "list": list,
            "nextPageUrl": np[1],
            "more": true,
            "success": true,
            "autoRequestMore": true,
        }
    }
    return {
        "list": list,
        //"updateTime": result.match(/更新时间正则/)[1]
    }
}
```
##### xpath目录解析
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let list = []
    let xml = params.nativeTool.XPathParserWithSource(result)
    let list_xpath = xml.queryWithXPath(`列表`)
    for (i in list_xpath) {
        list.push({
            "title": xpath(list_xpath[i], `//a`),
            "url": xpath(list_xpath[i], `//a/@href`),
        })
    }
    let np = xpath(xml, ``)
    if (np != undefined){
        return {
            "list": list,
            "nextPageUrl": np,
            "more": true,
            "success": true,
            "autoRequestMore": true,
        }
    }
    return {
        "list": list,
        "updateTime": xpath(xml, ``),
    }
    function xpath(item, path) {
        if (path.length > 0 && item.queryWithXPath(path).length > 0) {
            return item.queryWithXPath(path)[0].content()
        }
        return undefined
    }
}
```
#### 3.隐藏链接
##### 隐藏章节链接 -- 默默中文（正则+xpath处理）
><font face='KaiTi' >将隐藏链接替换成标准格式并标记  
>将已标记的链接替换成上一章链接+标记，用于内容页识别并处理</font>

```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    var list = []
    //替换隐藏链接为标准格式
    result = result.replace(/>(.*?)\(.*?\)/gim, `><a href="todo">$1</a>`)
    //xpath解析列表
    let xml = params.nativeTool.XPathParserWithSource(result)
    let list_xpath = xml.queryWithXPath(`//*[@class="booklist-top"]//li`)
    for (i in list_xpath) {
        let info = {}
        xpath(list_xpath[i], `//a`)
        info.title = xpath(list_xpath[i], `//a`)
        info.url = xpath(list_xpath[i], `//a/@href`)
        //替换隐藏章节url为上一章url
        if (info.url == "todo") {
            info.url = list[list.length - 1].url + "#todo"
        }
        list.push(info)
    }
    //目录页的下一页 
    let np = xpath(xml, `//a[contains(., "下一页")]/@href`)
    if (np == undefined) {
        return {
            "list": list,
        }
    }
    return {
        "list": list,
        "nextPageUrl": np,
        "more": true,
        "success": true,
        "autoRequestMore": true,
    }
    function xpath(item, path) {
        if (path.length > 0 && item.queryWithXPath(path).length > 0) {
            return item.queryWithXPath(path)[0].content()
        }
        return undefined
    }
}
```

##### 隐藏章节链接 -- 海棠书屋（正则处理）
><font face='KaiTi' >将隐藏链接替换成标准格式并标记  
>将已标记的链接替换成上一章链接+标记，用于内容页识别并处理</font>

```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    var list = []
    //替换隐藏目录结构
    result = result.replace(/<li>([^<].*?)<\/a><\/li/gi, `<li><a href="todo">$1</a></li`)
    let reg = /li><a .*?"(.*?)">(.*?)<.a>/gi
    while (tem = reg.exec(result)) {
        let info = {}
        info.title = tem[2]
        info.url = params.queryInfo.detailUrl + tem[1]
        //替换隐藏章节url为上一章url
        if (/todo/.test(info.url)) {
            info.url = list[list.length - 1].url + "#todo"
        }
        list.push(info)
    }
    //目录页的下一页 
    let np = result.match(/下一页正则/)
    if (np.length > 0) {
        return {
            "list": list,
            "nextPageUrl": np[1],
            "more": true,
            "success": true,
            "autoRequestMore": true,
        }
    }
    return {
        "list": list,
    }
}
```
##### 没有目录情况（填个list传下去）
```javascript
function functionName(config, params, result) {
    return {
        "list": [
            {
                "title": "随便",   //params.queryInfo.bookName
                "url": params.queryInfo.detailUrl,
            }
        ]
    }
}
```

### 六、正文相关
#### 1.正文请求
##### 标准正文请求或空
```javascript
@js:

return {
    "url": result,
    //"webView": 1,
    //"forbidCookie": true,
    "httpHeaders": config.httpHeaders,
    "cacheTime": 600,
}
```
#### 2.标准解析
##### xpath正文解析
```javascript
function functionName(config, params, result) {
    
    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let con = ""
    let xml = params.nativeTool.XPathParserWithSource(result)
    let text_list = xml.queryWithXPath(`正文列表`)
    for (i in text_list) {
        con += text_list[i].content() + "\n"
    }
    let np = xml.queryWithXPath(`//a[contains(., "下一页")]`)
    if (np.length > 0) {
        return {
            "content": ad(con),
            "nextPageUrl": np[0].content(),
            "more": true,
            "success": true,
            "autoRequestMore": true
        }
    }
    return {
        "content": ad(con),
    }
    function ad(str){
        let ad_reg = /广告内容1|广告内容2/gi
        return str.replace(ad_reg, "")
    }
}
```
##### 正则正文解析方案1
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let start = `开始标识如<div id="content">`
    let end = `结束标识如</div>`
    let con = result.substr(result.indexOf(start) + start.length)
    con = con.substr(0, con.indexOf(end))
    let np = result.match(/下一页正则/)
    if (np.length > 0) {
        return {
            "content": ad(con),
            "nextPageUrl": np[1],
            "removeHtmlKeys": "content",
            "more": true,flase
            "success": true,
            "autoRequestMore": true
        }
    }
    return {
        "content": ad(con),
        "removeHtmlKeys": "content",
    }
    function ad(str) {
        let ad_reg = /广告内容1|广告内容2/gi
        return str.replace(ad_reg, "")
    }
}
```
##### 正则正文解析方案2
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    let con = result.match(/开始标识([\s|\S]*?)结束标识/)[1]
    let np = result.match(/下一页正则/)
    if (np.length > 0) {
        return {
            "content": ad(con),
            "nextPageUrl": np[1],
            "removeHtmlKeys": "content",
            "more": true,
            "success": true,
            "autoRequestMore": true
        }
    }
    return {
        "content": ad(con),
        "removeHtmlKeys": "content",
    }
    function ad(str) {
        let ad_reg = /广告内容1|广告内容2/gi
        return str.replace(ad_reg, "")
    }
}
```
#### 3.todo 翻页解析
##### todo 正则解析
```javascript
function functionName(config, params, result) {

    result = result.replace(/<meta.*?charset=.*?>/i, "")
    //正则解析下一章或下一页
    let np = result.match(/f="(.*?)">下一(.*?)<.a>/)
    if (/#todo/.test(params.responseUrl)) {
        //解析下一章URL传给下一页
        if (np[2] == "页") {
            np[1] += "#todo"
        }
        return {
            "nextPageUrl": np[1],
            "more": true,
            "success": true,
            "autoRequestMore": true,
        }
    }
    //解析内容页
    let con = result.match(/开始标识([\s|\S]*?)结束标识/)[1]
    let np = result.match(/下一页正则/)
    if (np.length > 0) {
        return {
            "content": ad(con),
            "nextPageUrl": np[1],
            "removeHtmlKeys": "content",
            "more": true,
            "success": true,
            "autoRequestMore": true
        }
    }
    return {
        "content": ad(con),
        "removeHtmlKeys": "content",
    }
    function ad(str) {
        let ad_reg = /广告内容1|广告内容2/gi
        return str.replace(ad_reg, "")
    }
}
```
##### todo xpath解析
```javascript
function functionName(config, params, result) {

    let con = ""
    let xml = params.nativeTool.XPathParserWithSource(result)
    if (/#todo/.test(params.responseUrl)) {
        let np = xpath(xml, `//a[contains(., "下一章")]/@href`)
        if (np == undefined) {
            np = xpath(xml, `//a[contains(., "下一页")]/@href`)
        }
        return {
            "nextPageUrl": np,
            "more": true,
            "success": true,
            "autoRequestMore": true,
        }
    }
    let text_list = xml.queryWithXPath(`//div[@id]/p`)
    for (i in text_list) {
        con += text_list[i].content() + "\n"
    }
    let np = xpath(xml, `//a[contains(., "下一页")]/@href`)
    if (np != undefined){
        return {
            "content": ad(con),
            "nextPageUrl": np,
            "more": true,
            "success": true,
            "autoRequestMore": true,
        }
    }
    return {
        "content": ad(con),
    }
    function xpath(item, path) {
        if (path.length > 0 && item.queryWithXPath(path).length > 0) {
            return item.queryWithXPath(path)[0].content()
        }
        return undefined
    }
    function ad(str) {
        let ad_reg = /广告内容1|广告内容2/gi
        return str.replace(ad_reg, "")
    }
}
```
#### 4.正文乱序范例
##### 99藏书网
```javascript
function functionName(config, params, result) {
    //解析内容页
    let xml = params.nativeTool.XPathParserWithSource(result)
    let txt_xpath = xml.queryWithXPath(`//div[@id="content"]/div`)
    //params.nativeTool.log("txt长度：" + txt_xpath.length)
    let key = result.match(/name="client.*?="(.*?)"/)
    let e = params.nativeTool.base64Decode(key[1]).split(/[A-Z]+%/)
    //params.nativeTool.log(e)
    let j = 0
    //乱序处理
    let con_list = []
    for (let i in e) {
        if (e[i] < 3) {
            con_list[e[i]] = txt_xpath[i].content()
            j++
        } else {
            con_list[e[i] - j] = txt_xpath[i].content()
            j = j + 2
        }
    }
    return {
        "content": ad(con_list.join("\n")),
    }
    function ad(str) {
        let ad_reg = /广告内容1|广告内容2/gi
        return str.replace(ad_reg, "")
    }
}
```
##### 版主123
```javascript
function functionName(config, params, result) {

    var con = ""
    //替换图片文字
    result = trs(result)
    //解析内容页
    let xml = params.nativeTool.XPathParserWithSource(result)
    let txt_xpath = xml.queryWithXPath(`//div[@id][@class]//div[@id][@class]//p`)
    //params.nativeTool.log("txt长度：" + txt_xpath.length)
    if (params.responseUrl.indexOf("_2") == -1) {
        //除了第二页的情况
        for (i in txt_xpath) {
            con += txt_xpath[i].content() + "\n\n"
        }
    } else {
        //第二页情况，乱序处理，页面取2个key值
        let key = result.match(/client".*?="(.*?)"[\s|\S]*?codeurl="(\d+)"/)
        let e = params.nativeTool.base64Decode(key[1]).split(/[A-Z]+%/)
        //抄页面处理排序 （article.js的UpWz函数）
        let con_list = []
        for (i in e) {
            let k = Math.ceil(Number(e[i]) - Math.ceil((Number(i) + 1) % Number(key[2])))
            //params.nativeTool.log("code：" + key[2] + "\ne值：" + e + "\nm值：" + e[i] + "\ni值：" + i + "\nk值：" + k + "\ntxt：" + txt_xpath[i].content())
            con_list[k] = txt_xpath[i].content()
        }
        con = con_list.join("\n\n")
    }
    //解析下一页
    let np = xml.queryWithXPath(`//a[@class="curr"]/following-sibling::a/@href`)
    if (np.length > 0) {
        np = np[0].content()
        if(/java/.test(np)) {
            let tem = np.match(/\((.*?),(.*?),(.*?),(.*?)\)/)
            np = `/${tem[1]}/${tem[2]}/${tem[3]}_${tem[4]}.html`
        }
        return {
            "content": ad(con),
            "nextPageUrl": np,
            "more": true,
            "success": true,
            "autoRequestMore": true,
        }
    }
    return {
        "content": ad(con),
    }
    function ad(str) {
        let ad_reg = /广告内容1|广告内容2/gi
        return str.replace(ad_reg, "")
    }
    function trs(str) {
        let data = {'LioClJ': '体', '2NZ49v': '流', 'ELBd6f': '口', '6dG8O3': '胯', 'U6Z5wl': '露', 'BQEdki': '精', 'FZjTfQ': '潮', 'ol0PKI': '腿', '9UCAQf': '子', 'BXLfhV': '性', 'UPKJkj': '根', '2AMaij': '敏', 'sJW11m': '骚', 'sVM2Qf': '高', '62wAT9': '弄', 'eSTXxY': '穴', 'iphZH2': '子', 'wrWW0I': '裸', 'YnGqSD': '强', 'VrtVir': '擦', 'v6hpZK': '处', 'GcbZ5D': '吹', 'sStAd6': '根', 'l4CgSl': '进', 'blNSnb': '高', '2IAUyo': '体', 'laxcov': '顶', 'a0A63i': '欲', 'bfI8kL': '色', 'A4Qfvt': '热', 'vljEVE': '感', 'OyMcY4': '情', 'qGCZTU': '出', 'dAL6t6': '股', 'MjsYMZ': '逼', '42cp7O': '性', 'zteiXQ': '干', 'KzrjsT': '伦', 'RadXt3': '皮', 'T0v94f': '道', 'lAIyGS': '跳', 's7iRyY': '口', 'wQv04h': '芮', '4fB9tq': '露', 'ZnAdae': '感', 'CO8zut': '荡', 'k7j7H8': '情', 'cLYaLR': '骚', '9nsnIN': '干', 'U6ETuf': '道', '1Gqg5n': '水', 'TUkVzq': '塞', 'EmvIwd': '点', '1ZeSOp': '强', 'JIlF24': '出', 'DdpONj': '出', 'JpT1Mm': '拔', 'fgPYET': '裤', 'NQv357': '顶', 'Q9pFnf': '内', 'serxXI': '身', 'b5wzXI': '物', '495z5g': '身', 'KsqvDd': '爱', 'chEI2r': '射', 'cJwvTO': '精', 'HBvy8e': '子', 'i7grsU': '体', 'lq8Nt7': '爱', 'BGhRQs': '干', 'gQDwm1': '芮', 'en8f2a': '弄', 'zmgJLS': '做', '1Rqmzv': '进', 'APKJeI': '射', 'c26SPT': '股', 'DQQWSq': '胯', '5l7wMB': '裤', 'OrdXOa': '粗', 'X3zaV8': '弄', 'wiv1sf': '裸', 'ERPKzJ': '下', 'KgRlsy': '蛋', '76cofH': '穴', 'bVdhJz': '下', 'YTLWqK': '穴', 'I1pnu0': '按', 'FQf3kh': '情', 'xc9oWl': '做', 'tc0YA4': '荡', 'cOfw1e': '芮', 'vs1Plo': '点', 'B9ibk9': '腿', 'EW5hm5': '液', 'FuJOia': '潮', 'DzGDxZ': '壁', 'ilPY6c': '子', 'pHF53O': '内', 'N7nFKM': '头', 't30G4a': '进', 'LEaYe0': '头', 'p1PGd7': '硬', 'qqEApa': '感', 'kkcpDo': '穴', 'SUtjYj': '高', '5FEG8Q': '揉', 'CpYgjA': '情', 'cxb6eb': '润', 'yWeWm4': '做', 'kKo4g3': '润', 'EuAtBp': '龟', 'EJSYgs': '揉', 'ZqRaUS': '壁', 'Lyiatc': '根', '2po3tg': '挺', 'AMzQIa': '洞', '7R2MBk': '流', 'lRYIUO': '部', 'du1N5w': '热', 'BtUHWu': '部', 'vQ0UkO': '茎', 'PHBYiz': '吟', 'WEdPjy': '部', '6uLPPJ': '含', 'YTDoKI': '处', 'SkEy20': '滑', 'ihBTEG': '器', 'TaNl54': '下', 'o0c50b': '进', 'vOQlC3': '马', '0Hrf9j': '拔', 'LpI3Ya': '茎', 'BK7bSX': '水', 'VSybgj': '舌', 'AGSeds': '出', 'Gcd4r1': '色', '3pvbHR': '粗', 'MB0xV6': '裤', 'IC8rPT': '舌', 'qt7nob': '马', 'U6WN3k': '入', '7XNrcn': '裤', 'lSikq4': '洞', 'bgtzVL': '缝', 'fn3SAp': '性', 'S9opou': '擦', 'PV4Hqe': '潮', 'xlro17': '流', 'dxlfkh': '点', 'REI2Ss': '痒', 'M5Y2Vj': '胸', 'B3uyZi': '口', 'snP28t': '芮', 'GygYZo': '胸', 'NjzTh4': '内', 'IkQkE5': '赫', 'ksWEHN': '液', 'iakMxG': '吮', '7CuhmP': '入', 'RTdyb8': '胯', 'i14WSg': '顶', 'cC6Vq3': '吟', 'SAZ5aD': '吹', 'sJkTZh': '裸', 'qdv1O1': '弄', 'dVDBcz': '精', 'Vvdl2x': '头', 'nLH5KM': '喘', 'J0166n': '高', 'cyXxkL': '硬', 'I8L8bp': '欢', 'FrOIwe': '软', 'y145O6': '龟', 'T0HGCo': '股', 'vpAPLS': '胸', 'yF02AP': '水', '4Yv80l': '欲', 'k96uX6': '吟', 'vP6o9L': '捏', 'hbDaMY': '体', 'gEi1jH': '身', '5uSX9g': '喷', 'f5Sl5W': '口', 'zBh4Mb': '挺', 'DSYjBY': '色', 'uLc4HM': '露', 'oXnHAc': '欲', 'ihBR1Q': '荡', '7iGkNp': '奶', '9GHmu4': '擦', 'x0ifDw': '感', 'pZdo2p': '配', 'OL7QcJ': '潮', 'BuJlIw': '奶', 'lEiXgL': '捏', 'rlXZQC': '内', 'CzCfHT': '道', '6tOIJK': '器', 'I1yMcq': '喷', 'NSWNMw': '色', 'P2Horw': '吹', 'icvlLW': '流', 'NT4eYi': '尿', 'qlS3Sx': '软', 'Yo3pKn': '液', 'fYrDFa': '茎', 'IfW5g5': '蒂', '45C3gz': '性', 'cbg3dK': '水', 'tgNIKD': '吹', 'ULN2GC': '裸', 'labvz6': '欢', 'HJCH2o': '吟', 'DyWqQS': '马', '5tYFdr': '吮', 'Jq36k0': '泌', 'soNwZr': '欢', 't3C8zJ': '洞', 'ui0IuL': '慰', 'OB2TTN': '挺', 'cUJHxq': '股', 'WkQxtq': '勃', 'Csqmh8': '头', '3hWTSj': '腿', 'mQRToV': '身', '8x1iD4': '器', 'HPdJdE': '奶', 'tqMZoS': '赫', 'cZFN2F': '下', 'gTFRHz': '根', 'Jsh8cM': '射', 'aTpwyQ': '处', 'VJhKzQ': '骚', 'hz892M': '处', 'c2D1cS': '泌', '3Ojemu': '射', 'v1KpJ7': '胸', 'RePYYJ': '敏', 'HD4iVY': '痒', 'Agl4cK': '含', 'kQDZNr': '壁', 'wg1roK': '爱', 'ZA3CQl': '欲', 'Ish0FH': '勃', '7i4BzH': '精', 'uFWS3f': '操', '1PIiqD': '腺', 'RtxpTW': '龟', 'D95mg4': '勃', '9pCJPI': '皮', 'QNOOev': '雷', '0CFlIl': '含', 'MlPZuH': '爱', '4hBvGM': '肿', 'dv9meP': '具', 'g5nrVa': '奶', '1UVQIb': '挺', 'Q9u4Hn': '腿', 'tlzScv': '强', 'VY9zC1': '露', 'cvk50v': '勃', 'CmeBBJ': '喷', '0nq0VY': '干', 'rP73LX': '按', 'pcvyUw': '茎', 'FkuEYc': '物', 'lqBYN8': '粗', '6h1Wm5': '入', 'NtKGQj': '滑', '2kcOY4': '喘', 'AM1xdN': '蒂', 'eJuVHi': '舌', 'PfBmr2': '戴', 'bCqRn9': '骚', 'kLDO7O': '点', 'bXXtdD': '部', '5tIyci': '软', 'q5JsSG': '龟', '4qG1NW': '魔', 'Ijpxrq': '物', 'Fnu4Bq': '户', 'NLZtyZ': '赫', 'GfuyiM': '滑', 'EszBxk': '蛋', 'nXf7UP': '触', 'rzJGLj': '荡', 'Ok4qzK': '操', 'clF9yk': '润', 'ZKkjVL': '管', '8RAHon': '美', '1O95j2': '滑', 'E1EJAQ': '瓣', 'tsrxQm': '敏', 'ZTK6lV': '热', 'fzBwG0': '道', 'lWWYjB': '春', 'F1dUlS': '具', 'mfVI32': '逼', 'uwpnVP': '臀', 'YaZg09': '泄', 'JXHirG': '粗', '9OcLmy': '跳', '7KBXyo': '硬', 'jY124Z': '具', 'q6Z96G': '按', 'hKqeSx': '具', 'IOky3x': '喘', 't6RYkR': '敏', 'iQmeua': '做', '1X20Ty': '跳', '3Hwl1H': '美', '6HpCJE': '缝', 'WIPVZ4': '器', 'rTPCFr': '喷', '5SE8r3': '泄', 'ArDF2p': '户', 'RtxmnH': '娇', 'gzRWFd': '触', 'U0W9UH': '跳', 'QsbwjD': '拔', 'X8bbsB': '瘫', '66lPty': '痒', 'w5f3cQ': '入', '6eCcMP': '洞', 'x9QaDF': '舌', 'HhAfau': '蛋', '5yadiq': '塞', 'qgk8iF': '毛', '4aNpY9': '慰', 'U0GaWS': '按', 'Bh99m9': '美', 'Vm8RF6': '赫', 'YWkQCv': '慰', 'DAERk4': '顶', 'nHbr7A': '热', '2BRoU9': '硬', 'ZWHEyl': '喘', 'lKZcm0': '液', 'jpFqXS': '娇', 'AtG8KX': '欢', 'o5a6B7': '配', '484kLk': '魔', 'KIpRtH': '泄', 'jD5SND': '捏', 'qiQRjC': '胯', 'ZTjfuD': '揉', 'rv3jS1': '物', '6kXjY9': '吮', 'yIV2Zd': '吮', 'w91HjI': '烫', 'PohD4c': '润', 'rzYDmE': '蛋', '1sr79n': '坚', 'JpKmO2': '泄', 'eTYnW5': '邦', 'gDzupV': '缝', 'wYHuO9': '宫', 'HPGsw7': '妓', 'zWdASR': '柱', '5KlE9q': '逼', 'J2nQLO': '逼', 'M4y0ww': '皮', 'GE46uh': '坚', '22up19': '触', 'JQnh10': '肿', 'FCSRwT': '娇', 'vVJWmo': '强', '1kbQgJ': '触', 'zXbnhs': '塞', 'DRr5Zi': '臀', 'WKmkJt': '食', 'TALKff': '暖', 'xuBFbK': '壁', 'Tjx8nx': '宠', 'jGnJyk': '拔', 'GWC8b5': '滚', 'Tgho1H': '含', 'JqYuPd': '蒂', 'WgeHJh': '喉', 'pBvl3o': '管', 'IE2Lvr': '操', 'hX7VId': '肛', 'uZFjND': '瘫', 'G0bitc': '戴', 'VezNju': '奴', '47WXn9': '操', 'J11xNk': '缝', 'SOb2PL': '臀', 'vTJ57L': '春', 'sy1Gqu': '烫', 'Uz5uRT': '融', 'JAlCwD': '戴', 'sev4Lw': '配', 'kqaDY1': '戴', 'hF9P5B': '囊', 'BjjWSW': '柱', 'Y9Lpfz': '催', 'lew8w1': '捅', 'LBYtQ1': '捅', '57n62i': '伦', 'VQpKuW': '软', 'uroVNU': '瘫', 'gJBIjo': '瓣', 'u3KNRc': '魔', '6SJ8r8': '马', 'EBgJsY': '捏', 'TY1H6r': '户', '2Bim4y': '柱', '0yDKJc': '蒂', 'EBRtNJ': '春', 'I1G4ge': '催', 'Cf89ak': '美', 'dvdObQ': '肿', 'L6XTCm': '擦', 'ry48DM': '捅', 'bLdZI6': '魔', '23m3Dk': '户', 'HQlZW8': '殖', 'OZyn8s': '宫', '8WZqTV': '塞', 'WucpiX': '痒', 'eqfWNy': '暖', '5ByEAD': '慰', 'skXjer': '殖', 'in45ZS': '管', 'Q6iONP': '配', 'T1v7mT': '春', '5MJlvS': '宠', 'MdDxXR': '妓', 'P4rFI8': '食', 'fDcIsy': '肿', 'fFtkTA': '娇', 'a65cWx': '揉', 'zWQOYF': '囊', '8jKpGP': '邦', '3xEn8A': '臀', 'lHcxyV': '烫', 'QxMala': '毛', 'EvXHyx': '殖', '0mUvs7': '管', 'V6XABQ': '瘫', '3ZaKf5': '宫', 'lXzqGt': '喉', '6k43Sr': '宠', '5ypRua': '腺', 'igQinJ': '暖', 'GpcbYb': '泌', 'WGUwU1': '皮', 'fPaEnn': '伦', 'L4dDvm': '暖', '3RejmJ': '催', 'eDwYjm': '捅', 'kAsuRJ': '食', 'ZnDaS1': '食', 'glxiNX': '腺', 'QlNi54': '柱', 'Xh7gKv': '宫', 'gHEWsf': '坚', 'bCceR2': '虐', 'AMticx': '瓣', 'lTtRxE': '尿', 'xQQCgk': '伦', 'Qz3ed6': '腺', 'OIHwGS': '烫', 'XDJiLf': '滚', 'jRSpiZ': '喉', 'PXnuUM': '毛', 'CsYeGT': '肥', 'rq3idu': '葡', 'r6vGGp': '虐', 'Be8Kjb': '尿', '5VDCeX': '殖', 'lw97NE': '囊', 'JXCQTC': '融', 'HWMgSX': '肠', 'r7EHfn': '肥', 'hMd6kd': '融', 'PtiDrt': '蕊', 'en0rfl': '瓣', 'H6pC1x': '滚', 'LWh9YT': '坚', 'FncVEj': '萄', 'efGO2u': '喉', 'cep55R': '属', 'RdpiPv': '肥', '8Y36RN': '泌', 'KrfOpe': '催', '8n3R3m': '尿', '6dkaMc': '房', 'mI7NDt': '肠', 'zJ5TEx': '宠', 'WFDfmZ': '囊', 'kIJ63z': '丸', 'IuVe1K': '肠', 'nfRLm7': '脔', 'tbFzym': '邦', 'bruQOh': '核', 'qujoK7': '奴', 'nSHTuk': '奴', '2PnANQ': '雷', '8y1JVs': '丸', 'wq4fov': '雷', 'Oml9Gd': '丸', 'OUlWTO': '葡', 'GRYyRZ': '邦', 'nuG6Ks': '毛', 'o7S5d9': '虐', '3HEXEr': '滚', 'TGKDXO': '虐', 'jOA75k': '核', 'KDguTl': '核', 'wF2vul': '核', 'Qo3Etd': '肠', 'tf4oej': '雏', '7iWipZ': '萄', 'v7XU6L': '蕊', 'XrLMqV': '肥'}     
        str = str.replace(/<.?strong>/gi,"").replace(/<meta.*?charset=.*?>/i,"")
        for (let key in data) {
            let reg = new RegExp(`<img src="${config.host}/wzbodyimg/${key}.png">`, "g")
            str = str.replace(reg, data[key])
        }
        return str
    }
}
```
