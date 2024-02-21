#define _HASH #
#define HASH() _HASH
#define _SQUOTE '
#define SQUOTE() _SQUOTE
#define _DQUOTE "
#define DQUOTE() _DQUOTE
#define _CONCAT(a, ...) a##__VA_ARGS__
#define CONCAT(a, ...) _CONCAT(a, __VA_ARGS__)

#define END(...) </__VA_ARGS__>

#define A(LINK, ...)                            \
        <a href= ## LINK>__VA_ARGS__</a>

#define AID(ID, ...)                            \
        <a href=HASH()ID>__VA_ARGS__</a>

#define _P(COUNTER, ...)                                                \
        </p><p id=CONCAT(p_, COUNTER)><a href=HASH()CONCAT(p_, COUNTER)>¶</a>

#define P(...)                                  \
        _P(__COUNTER__, __VA_ARGS__)

#define H2(ID, ...)                             \
        <h2 id= ## ID>AID(ID, __VA_ARGS__)</h2>
#define H3(ID, ...)                             \
        <h3 id= ## ID>AID(ID, __VA_ARGS__)</h3>
#define H4(ID, ...)                             \
        <h4 id= ## ID>AID(ID, __VA_ARGS__)</h4>
#define H5(ID, ...)                             \
        <h5 id= ## ID>AID(ID, __VA_ARGS__)</h5>
#define H6(ID, ...)                             \
        <h6 id= ## ID>AID(ID, __VA_ARGS__)</h6>

#define IMG(LINK, ...)                          \
        <img src=LINK alt=#__VA_ARGS__/>
#define AUDIO(LINK, ...)                                \
        <audio src=LINK controls __VA_ARGS__></audio>
#define VIDEO(LINK, ...)                                \
        <video src=LINK controls __VA_ARGS__></video>

#define FIG()       <figure>
#define FIGCAP(...) <figcaption>__VA_ARGS__</figcaption></figure>

#define PRE()       FIG()<pre><code>
#define PRECAP(...) </code></pre>FIGCAP(__VA_ARGS__)

#define BQ()        FIG()<blockquote>
#define BQCAP(...)  </blockquote>FIGCAP(__VA_ARGS__)

#define DD(...)                                 \
        </dd><dt> __VA_ARGS__ </dt> <dd>
#define DL(...)                                 \
        <DL><dt> __VA_ARGS__ </dt> <dd>

#define LI      </li><li>
#define UL(...) <UL __VA_ARGS__><li>
#define ULI <UL><li>
#define OL(...) <OL __VA_ARGS__><li>
#define OLI <OL><li>

#define CD(...)                                 \
        <code>__VA_ARGS__</code>

#define VAR(...)                                \
        <var>__VA_ARGS__</var>

#define TD(...) <td>__VA_ARGS__</td>
#define TH(...) <th>__VA_ARGS__</th>

#define TAB(...)                   <TABLE> <tr> <th> __VA_ARGS__ </th>
#define TAB2(ONE, ...)             <TABLE> <tr> <th> ONE </th> <th> __VA_ARGS__ </th>
#define TAB3(ONE, TWO, ...)        <TABLE> <tr> <th> ONE </th> <th> TWO </th> <th> __VA_ARGS__ </th>
#define TAB4(ONE, TWO, THREE, ...) <TABLE> <tr> <th> ONE </th> <th> TWO </th> <th> THREE </th> <th> __VA_ARGS__ </th>
#define TAB5(ONE, TWO, THREE, FOUR, ...) <TABLE> <tr> <th> ONE </th> <th> TWO </th> <th> THREE </th> <th> FOUR </th> <th> __VA_ARGS__ </th>

#define TRO(...)                   </tr> <tr> <td> __VA_ARGS__ </td>
#define TRO2(ONE, ...)             </tr> <tr> <td> ONE </td> <td> __VA_ARGS__ </td>
#define TRO3(ONE, TWO, ...)        </tr> <tr> <td> ONE </td> <td> TWO </td> <td> __VA_ARGS__ </td>
#define TRO4(ONE, TWO, THREE, ...) </tr> <tr> <td> ONE </td> <td> TWO </td> <td> THREE </td> <td> __VA_ARGS__ </td>
#define TRO4(ONE, TWO, THREE, ...) </tr> <tr> <td> ONE </td> <td> TWO </td> <td> THREE </td> <td> __VA_ARGS__ </td>

#define TABCAP(...) <caption>__VA_ARGS__</caption></TABLE>

#define SECTION2(ID, ...)                                       \
        </section></section></section></section></SECTION>      \
        <SECTION id= ## ID><h2>AID(ID, __VA_ARGS__)</h2>
#define SECTION3(ID, ...)                                       \
        <SECTION id= ## ID><h3>AID(ID, __VA_ARGS__)</h3>
#define SECTION4(ID, ...)                                       \
        <SECTION id= ## ID><h4>AID(ID, __VA_ARGS__)</h4>
#define SECTION5(ID, ...)                                       \
        <SECTION id= ## ID><h5>AID(ID, __VA_ARGS__)</h5>
#define SECTION6(ID, ...)                                       \
        <SECTION id= ## ID><h6>AID(ID, __VA_ARGS__)</h6>

#define INPUT(TYPE, ...) <input type=TYPE __VA_ARGS__ />

#define OPT(VAL) <option value=VAL>VAL</option>

#define DETAILS(...) <DETAILS> <summary> __VA_ARGS__ </summary>

#define MATH(...) <MATH __VA_ARGS__>
#define MI(...)   <mi>__VA_ARGS__</mi>
#define MO(...)   <mo>__VA_ARGS__</mo>
#define MOLR(LSPACE, RSPACE, ...) <mo lspace=LSPACE rspace=RSPACE>__VA_ARGS__</mo>
#define MN(...)   <mn>__VA_ARGS__</mn>
#define MRO(...)  <mrow>__VA_ARGS__</mrow>
#define MSUB(BASE, ...)         <msub>BASE __VA_ARGS__</msub>
#define MSUP(BASE, ...)         <msup>BASE __VA_ARGS__</msup>
#define MSUBSUP(BASE, SUB, ...) <msubsup>BASE SUB __VA_ARGS__</msubsup>
#define MFRAC() <MFRAC>
#define MFRACSEP()
