% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-define(EMILIO_CHECKS, [
    emilio_check_line_length,
    emilio_check_indents_counts,
    emilio_check_indents,
    emilio_check_indents_match,
    emilio_check_indents_clauses,
    emilio_check_indents_clause_bodies,
    emilio_check_indents_when,
    emilio_check_indents_op2,
    emilio_check_indents_exports,
    emilio_check_ws_spaces_only,
    emilio_check_ws_file_newline,
    emilio_check_ws_commas,
    emilio_check_ws_attributes,
    emilio_check_ws_misc,
    emilio_check_ws_parens,
    emilio_check_ws_lists,
    emilio_check_ws_tuples,
    emilio_check_nl_top_level,
    emilio_check_nl_fun_clauses,
    emilio_check_exports_decl,
    emilio_check_exports_groups,
    emilio_check_exports_order,
    emilio_check_anti_underscores,
    emilio_check_logic_case
]).


-define(EMILIO_FILE_KEY, emilio_curr_file).


-define(EMILIO_REPORT(Anno, Code),
        emilio_report:update(
                get(?EMILIO_FILE_KEY), ?MODULE, Anno, Code, undefined)).

-define(EMILIO_REPORT(Anno, Code, Arg),
        emilio_report:update(
                get(?EMILIO_FILE_KEY), ?MODULE, Anno, Code, Arg)).
