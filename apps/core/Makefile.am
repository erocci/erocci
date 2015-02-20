# Copyright 2015 Jean Parpaillon, all rights reserved
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
erlang_APP = occi_core
erlang_MODULES = \
	occi_acl \
	occi_app \
	occi_config \
	occi \
	occi_hook \
	occi_known \
	occi_listener \
	occi_store \
	occi_sup \
	occi_table_mgr \
	backend/occi_backend_dummy \
	backend/occi_backend \
	model/occi_action \
	model/occi_capabilities \
	model/occi_cid \
	model/occi_entity \
	model/occi_kind \
	model/occi_mixin \
	model/occi_request \
	model/occi_type \
	model/occi_attribute \
	model/occi_category_mgr \
	model/occi_collection \
	model/occi_extension \
	model/occi_link \
	model/occi_node \
	model/occi_resource \
	model/occi_uri \
	rendering/occi_parser \
	rendering/occi_parser_occi \
	rendering/occi_renderer_json_tests \
	rendering/occi_renderer_uri_list \
	rendering/occi_parser_plain \
	rendering/occi_parser_xml \
	rendering/occi_renderer_occi \
	rendering/occi_renderer_xml \
	rendering/occi_parser_json0 \
	rendering/occi_parser_tests \
	rendering/occi_renderer \
	rendering/occi_renderer_plain \
	rendering/occi_scanner_json \
	rendering/occi_parser_json \
	rendering/occi_parser_text \
	rendering/occi_renderer_json \
	rendering/occi_renderer_text
erlang_FIRST = \
	occi_listener \
	backend/occi_backend \
	rendering/occi_renderer
erlang_HRL = \
	occi \
	occi_acl \
	occi_log \
	occi_model_store \
	occi_xml

EXTRA_DIST = \
	src/rendering/occi_parser_text.hrl \
	src/rendering/occi_parser.hrl

include $(top_srcdir)/erlang.mk
