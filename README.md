# oS-parser
## Things this presupposes
Chat logs (or Reddit subreddit threads) encoded as JSON files inside the specified directory. Some useful apps or services for extracting them:
* Discord: https://github.com/Tyrrrz/DiscordChatExporter
* Telegram: simply use the built-in "Export chat" feature from Telegram Desktop
* Reddit: https://pushshift.io/api-parameters/
## How to use
1. clone this repository.
2. `stack build`.
3. `stack exec -- oS-parser-exe "/path/to/directory/" "name_of_service"`.
    * examples: 
    * `stack exec -- oS-parser-exe "/usr/me/oS/my_telegram_chat_logs/" "telegram"`
    * `stack exec -- oS-parser-exe "/usr/me/oS/my_discord_chat_logs/" "discord"`
    * `stack exec -- oS-parser-exe "/usr/me/oS/my_subreddit_threads_logs/" "telegram"`
