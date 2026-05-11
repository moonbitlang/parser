///|
enum Large {
  E1
  E2(String)
  E3
  E4
  E5
  E6
  E7
  E8
  E9
  E10
  E11
  E12
  E13
  E14
  E15
  E16
  E17
  E18
  E19
  E20
  E21
  E22
  E23
  E24
  E25
  E26
  E27
  E28
  E29
  E30
  E31
  E32
  E33
  E34
  E35
  E36
  E37
  E38
  E39
  E40
  E41
  E42
  E43
  E44
  E45
  E46
  E47
  E48
  E49
  E50
  E51
  E52
  E53
  E54
  E55
  E56
  E57
  E58
  E59
  E60
  E61
  E62
  E63
  E64
  E65
  E66
  E67
  E68
  E69
  E70
  E71
  E72
  E73
  E74
  E75
  E76
  E77
  E78
  E79
  E80
  E81
  E82
  E83
  E84
  E85
  E86
  E87
  E88
  E89
  E90
  E91
  E92
  E93
  E94
  E95
  E96
  E97
  E98
  E99
  E100
  E101
  E102
  E103
  E104
  E105
  E106
  E107
  E108
  E109
  E110
  E111
  E112
  E113
  E114
  E115
  E116
  E117
  E118
  E119
  E120
  E121
  E122
  E123
  E124
  E125
  E126
  E127
  E128
  E129
  E130
  E131
  E132
  E133
  E134
  E135
  E136
  E137
  E138
  E139
  E140
  E141
  E142
  E143
  E144
  E145
  E146
  E147
  E148
  E149
  E150
  E151
  E152
  E153
  E154
  E155
  E156
  E157
  E158
  E159
  E160
  E161
  E162
  E163
  E164
  E165
  E166
  E167
  E168
  E169
  E170
  E171
  E172
  E173
  E174
  E175
  E176
  E177
  E178
  E179
  E180
  E181
  E182
  E183
  E184
  E185
  E186
  E187
  E188
  E189
  E190
  E191
  E192
  E193
  E194
  E195
  E196
  E197
  E198
  E199
  E200
  E201
  E202
  E203
  E204
  E205
  E206
  E207
  E208
  E209
  E210
  E211
  E212
  E213
  E214
  E215
  E216
  E217
  E218
  E219
  E220
  E221
  E222
  E223
  E224
  E225
  E226
  E227
  E228
  E229
  E230
  E231
  E232
  E233
  E234
  E235
  E236
  E237
  E238
  E239
  E240
  E241
  E242
  E243
  E244
  E245
  E246
  E247
  E248
  E249
  E250
  E251
  E252
  E253
  E254
  E255
  E256
  E257(Int)
  E258
} derive( Eq)

///|
let e1 : Large = E1

///|
let e257 : Large = E257(42)

///|
let e2 : Large = E2("abc")

///|
let e258 : Large = E258

///|
fn incorrcet_behavior(x : Large) -> Bool {
  println(x)
  x == E1
}

///|
fn init {
  println(incorrcet_behavior(e257))
  println(incorrcet_behavior(e1))
}

///|
fn segfault(x : Large) -> Unit {
  guard x is E2(s) else {  }
  println(s)
}

///|
fn init {
  segfault(e2)
  segfault(e258)
}

impl Show for Large with output(self, logger) {
  match self {
    E1 => logger.write_string("E1")
    E2(v0) => {
      logger.write_string("E2(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    E3 => logger.write_string("E3")
    E4 => logger.write_string("E4")
    E5 => logger.write_string("E5")
    E6 => logger.write_string("E6")
    E7 => logger.write_string("E7")
    E8 => logger.write_string("E8")
    E9 => logger.write_string("E9")
    E10 => logger.write_string("E10")
    E11 => logger.write_string("E11")
    E12 => logger.write_string("E12")
    E13 => logger.write_string("E13")
    E14 => logger.write_string("E14")
    E15 => logger.write_string("E15")
    E16 => logger.write_string("E16")
    E17 => logger.write_string("E17")
    E18 => logger.write_string("E18")
    E19 => logger.write_string("E19")
    E20 => logger.write_string("E20")
    E21 => logger.write_string("E21")
    E22 => logger.write_string("E22")
    E23 => logger.write_string("E23")
    E24 => logger.write_string("E24")
    E25 => logger.write_string("E25")
    E26 => logger.write_string("E26")
    E27 => logger.write_string("E27")
    E28 => logger.write_string("E28")
    E29 => logger.write_string("E29")
    E30 => logger.write_string("E30")
    E31 => logger.write_string("E31")
    E32 => logger.write_string("E32")
    E33 => logger.write_string("E33")
    E34 => logger.write_string("E34")
    E35 => logger.write_string("E35")
    E36 => logger.write_string("E36")
    E37 => logger.write_string("E37")
    E38 => logger.write_string("E38")
    E39 => logger.write_string("E39")
    E40 => logger.write_string("E40")
    E41 => logger.write_string("E41")
    E42 => logger.write_string("E42")
    E43 => logger.write_string("E43")
    E44 => logger.write_string("E44")
    E45 => logger.write_string("E45")
    E46 => logger.write_string("E46")
    E47 => logger.write_string("E47")
    E48 => logger.write_string("E48")
    E49 => logger.write_string("E49")
    E50 => logger.write_string("E50")
    E51 => logger.write_string("E51")
    E52 => logger.write_string("E52")
    E53 => logger.write_string("E53")
    E54 => logger.write_string("E54")
    E55 => logger.write_string("E55")
    E56 => logger.write_string("E56")
    E57 => logger.write_string("E57")
    E58 => logger.write_string("E58")
    E59 => logger.write_string("E59")
    E60 => logger.write_string("E60")
    E61 => logger.write_string("E61")
    E62 => logger.write_string("E62")
    E63 => logger.write_string("E63")
    E64 => logger.write_string("E64")
    E65 => logger.write_string("E65")
    E66 => logger.write_string("E66")
    E67 => logger.write_string("E67")
    E68 => logger.write_string("E68")
    E69 => logger.write_string("E69")
    E70 => logger.write_string("E70")
    E71 => logger.write_string("E71")
    E72 => logger.write_string("E72")
    E73 => logger.write_string("E73")
    E74 => logger.write_string("E74")
    E75 => logger.write_string("E75")
    E76 => logger.write_string("E76")
    E77 => logger.write_string("E77")
    E78 => logger.write_string("E78")
    E79 => logger.write_string("E79")
    E80 => logger.write_string("E80")
    E81 => logger.write_string("E81")
    E82 => logger.write_string("E82")
    E83 => logger.write_string("E83")
    E84 => logger.write_string("E84")
    E85 => logger.write_string("E85")
    E86 => logger.write_string("E86")
    E87 => logger.write_string("E87")
    E88 => logger.write_string("E88")
    E89 => logger.write_string("E89")
    E90 => logger.write_string("E90")
    E91 => logger.write_string("E91")
    E92 => logger.write_string("E92")
    E93 => logger.write_string("E93")
    E94 => logger.write_string("E94")
    E95 => logger.write_string("E95")
    E96 => logger.write_string("E96")
    E97 => logger.write_string("E97")
    E98 => logger.write_string("E98")
    E99 => logger.write_string("E99")
    E100 => logger.write_string("E100")
    E101 => logger.write_string("E101")
    E102 => logger.write_string("E102")
    E103 => logger.write_string("E103")
    E104 => logger.write_string("E104")
    E105 => logger.write_string("E105")
    E106 => logger.write_string("E106")
    E107 => logger.write_string("E107")
    E108 => logger.write_string("E108")
    E109 => logger.write_string("E109")
    E110 => logger.write_string("E110")
    E111 => logger.write_string("E111")
    E112 => logger.write_string("E112")
    E113 => logger.write_string("E113")
    E114 => logger.write_string("E114")
    E115 => logger.write_string("E115")
    E116 => logger.write_string("E116")
    E117 => logger.write_string("E117")
    E118 => logger.write_string("E118")
    E119 => logger.write_string("E119")
    E120 => logger.write_string("E120")
    E121 => logger.write_string("E121")
    E122 => logger.write_string("E122")
    E123 => logger.write_string("E123")
    E124 => logger.write_string("E124")
    E125 => logger.write_string("E125")
    E126 => logger.write_string("E126")
    E127 => logger.write_string("E127")
    E128 => logger.write_string("E128")
    E129 => logger.write_string("E129")
    E130 => logger.write_string("E130")
    E131 => logger.write_string("E131")
    E132 => logger.write_string("E132")
    E133 => logger.write_string("E133")
    E134 => logger.write_string("E134")
    E135 => logger.write_string("E135")
    E136 => logger.write_string("E136")
    E137 => logger.write_string("E137")
    E138 => logger.write_string("E138")
    E139 => logger.write_string("E139")
    E140 => logger.write_string("E140")
    E141 => logger.write_string("E141")
    E142 => logger.write_string("E142")
    E143 => logger.write_string("E143")
    E144 => logger.write_string("E144")
    E145 => logger.write_string("E145")
    E146 => logger.write_string("E146")
    E147 => logger.write_string("E147")
    E148 => logger.write_string("E148")
    E149 => logger.write_string("E149")
    E150 => logger.write_string("E150")
    E151 => logger.write_string("E151")
    E152 => logger.write_string("E152")
    E153 => logger.write_string("E153")
    E154 => logger.write_string("E154")
    E155 => logger.write_string("E155")
    E156 => logger.write_string("E156")
    E157 => logger.write_string("E157")
    E158 => logger.write_string("E158")
    E159 => logger.write_string("E159")
    E160 => logger.write_string("E160")
    E161 => logger.write_string("E161")
    E162 => logger.write_string("E162")
    E163 => logger.write_string("E163")
    E164 => logger.write_string("E164")
    E165 => logger.write_string("E165")
    E166 => logger.write_string("E166")
    E167 => logger.write_string("E167")
    E168 => logger.write_string("E168")
    E169 => logger.write_string("E169")
    E170 => logger.write_string("E170")
    E171 => logger.write_string("E171")
    E172 => logger.write_string("E172")
    E173 => logger.write_string("E173")
    E174 => logger.write_string("E174")
    E175 => logger.write_string("E175")
    E176 => logger.write_string("E176")
    E177 => logger.write_string("E177")
    E178 => logger.write_string("E178")
    E179 => logger.write_string("E179")
    E180 => logger.write_string("E180")
    E181 => logger.write_string("E181")
    E182 => logger.write_string("E182")
    E183 => logger.write_string("E183")
    E184 => logger.write_string("E184")
    E185 => logger.write_string("E185")
    E186 => logger.write_string("E186")
    E187 => logger.write_string("E187")
    E188 => logger.write_string("E188")
    E189 => logger.write_string("E189")
    E190 => logger.write_string("E190")
    E191 => logger.write_string("E191")
    E192 => logger.write_string("E192")
    E193 => logger.write_string("E193")
    E194 => logger.write_string("E194")
    E195 => logger.write_string("E195")
    E196 => logger.write_string("E196")
    E197 => logger.write_string("E197")
    E198 => logger.write_string("E198")
    E199 => logger.write_string("E199")
    E200 => logger.write_string("E200")
    E201 => logger.write_string("E201")
    E202 => logger.write_string("E202")
    E203 => logger.write_string("E203")
    E204 => logger.write_string("E204")
    E205 => logger.write_string("E205")
    E206 => logger.write_string("E206")
    E207 => logger.write_string("E207")
    E208 => logger.write_string("E208")
    E209 => logger.write_string("E209")
    E210 => logger.write_string("E210")
    E211 => logger.write_string("E211")
    E212 => logger.write_string("E212")
    E213 => logger.write_string("E213")
    E214 => logger.write_string("E214")
    E215 => logger.write_string("E215")
    E216 => logger.write_string("E216")
    E217 => logger.write_string("E217")
    E218 => logger.write_string("E218")
    E219 => logger.write_string("E219")
    E220 => logger.write_string("E220")
    E221 => logger.write_string("E221")
    E222 => logger.write_string("E222")
    E223 => logger.write_string("E223")
    E224 => logger.write_string("E224")
    E225 => logger.write_string("E225")
    E226 => logger.write_string("E226")
    E227 => logger.write_string("E227")
    E228 => logger.write_string("E228")
    E229 => logger.write_string("E229")
    E230 => logger.write_string("E230")
    E231 => logger.write_string("E231")
    E232 => logger.write_string("E232")
    E233 => logger.write_string("E233")
    E234 => logger.write_string("E234")
    E235 => logger.write_string("E235")
    E236 => logger.write_string("E236")
    E237 => logger.write_string("E237")
    E238 => logger.write_string("E238")
    E239 => logger.write_string("E239")
    E240 => logger.write_string("E240")
    E241 => logger.write_string("E241")
    E242 => logger.write_string("E242")
    E243 => logger.write_string("E243")
    E244 => logger.write_string("E244")
    E245 => logger.write_string("E245")
    E246 => logger.write_string("E246")
    E247 => logger.write_string("E247")
    E248 => logger.write_string("E248")
    E249 => logger.write_string("E249")
    E250 => logger.write_string("E250")
    E251 => logger.write_string("E251")
    E252 => logger.write_string("E252")
    E253 => logger.write_string("E253")
    E254 => logger.write_string("E254")
    E255 => logger.write_string("E255")
    E256 => logger.write_string("E256")
    E257(v0) => {
      logger.write_string("E257(")
      Show::output(v0, logger)
      logger.write_string(")")
    }
    E258 => logger.write_string("E258")
  }
}
