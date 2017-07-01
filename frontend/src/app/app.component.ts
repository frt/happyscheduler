import { Component } from '@angular/core';
import { InfoMessagesService } from './info-messages.service';

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html',
    styleUrls: ['./app.component.css']
})
export class AppComponent {

    constructor(public messagesService: InfoMessagesService) {}

}
